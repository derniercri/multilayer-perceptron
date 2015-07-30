-module(trainer).
-compile(export_all).

trainer(Trainer_pid, Inputs_network, Training_values, Training_constants) ->
    trainer(Trainer_pid, Inputs_network, Training_values, Training_constants, [], 0).    

trainer(_, _, _, {_, _, _, I}, _, I) ->
    max;
trainer(Trainer_pid, Inputs_network, [], Training_constants, List, I) ->
    trainer(Trainer_pid, Inputs_network,utils:shuffle(List), Training_constants,[], I);

trainer(Trainer_pid, Inputs_network, [Training_values | Training_list], Training_constants, List, I) ->
    {Training_inputs, Training_outputs} = Training_values,
    New_training_constants = add_training_output(Training_constants, Training_outputs),
    Trainer_pid ! {train, self(), New_training_constants},
    F = fun ({Input, Val}, _) ->
		Input ! {input, Val},
		ok
	end,
    lists:foldl(F, ok, lists:zip(Inputs_network, Training_inputs)),
    receive
	_ ->
	    trainer(Trainer_pid, Inputs_network, Training_list, Training_constants, [Training_values | List], I + 1)
    end.
    
add_training_output (Training_constants, Training_output) ->
    {A, B, C, _} = Training_constants,
    {A, B, C, array:from_list(Training_output)}.


init(Network, Network_size, Input_list) ->
    Self = self(),
    Trainer = spawn(fun() -> train_init(Network, Network_size, Self, Input_list) end),
    io:format("init trainer~n"),
    receive
	ok ->     io:format("init trainer done~n"),Trainer
    end.


%% fonction initialisant les poids avec des valeurs aléatoire et connectant le trainer au neurones
%% dans Network les neurones sont représenté par des couples {PID, Nb_entrée}
train_init(Network, Network_size, Main, Input_list) ->
    N_inputs = lists:sum(Network_size),
    %% initialisation des poids de chaque resaux
    F = fun (_, _, {PID, N}) ->
		Weights = gen_weight(N),
		PID ! {update, Weights},
		PID ! {connect_output, self()},
		{PID, array:from_list(Weights)}
	end,
    New_network = matrix:map(F, Network),

    %% connection du trainer aux entrée du neurone
    F2 = fun(PID) -> PID ! {connect_output, self()} end,
    lists:foreach(F2, Input_list),

    %% io:format("init done~n"),
    Main ! ok,
    train(New_network, N_inputs, Network_size).
	
%% génère une liste de N nombre compris entre -0.5 et 0.5	
gen_weight(N) ->
    gen_weight([], N).

gen_weight(Acc, 0) -> Acc;
gen_weight(Acc, I) -> 
    New_Acc = [random:uniform() - 0.5 | Acc],
    gen_weight(New_Acc, I - 1).


%% fonctions d'entrainement du resaux de neuronne
%% se met en attente de donné d'entrainement
%% Network : tableau de Layer
%%    Layer : tableau de Neuron
%%        Neuron : couple {PID, Weights}
%%            Weights : tableau des poids du neurone (biais compris)
%% N_inputs : nombre de résultat attendus (même ceux de la couche d'entrée
%% Network_size : liste des taille de chaque couche en commençant par la couche de sortie, la couche d'entrée doit aussi être compté

train(Network, N_inputs, Network_size) ->
    receive
	{train, From, Training_constant} ->
	    {Threshold, _, Speed, Training_outputs} = Training_constant,
	    %% recuperation des sortie de chaque neurone
	    Inputs_matrix = wait_inputs(N_inputs, Network_size),

	    Real_outputs = array:get(0, Inputs_matrix),
	    case compute_error(Training_outputs, Real_outputs) of
		Error when Error >= Threshold ->
		    New_network = backpropagation(Speed, Training_outputs, Inputs_matrix, Network),
		    From ! updated,
		    train(New_network, N_inputs, Network_size);
		_ ->
		    From ! ok,
		    train(Network, N_inputs, Network_size)
	    end
    end.



backpropagation(Speed, Training_outputs, Inputs_matrix, Network) ->		
    %% definition d'une fontion permetant de créer le tableau des gradients pour chaque neurone
    Size = array:size(Network),
    F = fun (I, _, Matrix) when I =:= Size -> Matrix;
	    (I, Layer, Matrix) ->
		F2 = fun (0, _, Line) -> Line;
			 (J, Input, Line) ->
			     case I of
				 0 ->
				     %% io:format("compute_gradient_output args : ~p, ~p~n", [Input, array:get(J - 1, Training_outputs)]),
				     Val = compute_gradient_output(Input, array:get(J - 1, Training_outputs)),
				     array:set(J - 1, Val, Line);
				 _ ->
				     Previous_gradient = array:get(I - 1, Matrix),
				     Sum = sum(I, J, Previous_gradient, Network),
					 Val = compute_gradient_hiden(Input, Sum),
				     array:set(J - 1, Val, Line)
			     end
		     end,
		Size_line = array:size(Layer) - 1,
		Acc = array:new(Size_line),
		New_Layer = array:foldl(F2,Acc , Layer),
		array:set(I, New_Layer, Matrix)
	end,

    %% calcul des gradients. les gradients de chaque neurone posséde les meme coordonée que son neurone dans la matrice Network

    Gradients_matrix = array:foldl(F, array:new(Size), Inputs_matrix),

    %% definition d'une fonction permetant de mettre a jour les poids à partir de la matrice Network (en utilisant la matrice des gradients)
    F_map = fun(I, J, {PID, Weights}) ->
		    Gradient = matrix:get(I, J, Gradients_matrix),
		    F_map2 = fun (K, W) ->
				     Input = matrix:get(I+1, K, Inputs_matrix),

				     W + Gradient * Input * Speed
			     end,
		    New_weights = array:map(F_map2, Weights),
		    PID ! {update, array:to_list(New_weights)},
		    {PID, New_weights}
	    end,
    matrix:map(F_map, Network).


compute_error(Target_values, Value) ->
    L1 = array:to_list(Target_values),
    L2 = tl(array:to_list(Value)),

    F = fun ({A, B}, Acc) -> math:pow((A - B),2) + Acc end,
    lists:foldl(F, 0, lists:zip(L1, L2)).


		
%% met le trainer en attente des entrée du resaux
%% renvoi un tableau contenant les sortie de chaque neurone
%% N : nombre d'entrée attendut
%% Network_size : (voir plus haut)
wait_inputs(N, Network_size) ->
    %% on augmente de 1 la taille de chaque couche pour le biais
    F_map = fun(Size) -> Size + 1 end,
    New_size = lists:map(F_map, Network_size),
    wait_inputs(N, matrix:new_variable(length(New_size), New_size), 0).

wait_inputs(N, Matrix, N) -> 
    F = fun(_, Array) -> array:set(0, 1, Array) end,
    array:map(F, Matrix);
wait_inputs(N, Matrix, I) -> 
    receive
	{done, Result, {_, Layer, Rank}} ->
	    New_matrix = matrix:set(Layer, Rank, Result, Matrix),
	    %% io:format("receive ~p/~p from ~p,~p~n", [I+1, N, Layer, Rank]),
	    wait_inputs(N, New_matrix, I + 1)
    end.

%% calcul du gradient pour la couche de sortie
compute_gradient_output(Value, Target_value) ->
    Value * (1 - Value) * (Target_value - Value).

%% calcul du gradient pour les couches caché
compute_gradient_hiden(Value, Sum) ->
    Value * (1 - Value) * Sum.


sum(Layer, Rank, Previous_gradients, Network) ->
    L = array:get(Layer - 1, Network),
    F = fun(_, {_, Weight_array}) -> array:get(Rank, Weight_array) end,
    Previous_weights = array:map(F, L),
    dot_product(Previous_gradients, Previous_weights).

%% produit scalaire entre 2 tableaux
dot_product(A1, A2) ->
    L1 = array:to_list(A1),
    L2 = array:to_list(A2),
    F = fun({A, B}, Acc) -> 
		%% io:format("~p * ~p + ~p~n", [A, B, Acc]),
		(A * B) + Acc end,
    lists:foldl(F, 0, lists:zip(L1, L2)).
