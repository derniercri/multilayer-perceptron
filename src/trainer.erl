-module(trainer).
-compile(export_all).

%%entraine un resaux (représenté par ses entrée et ses sortie).
%% Trainer_pid : pid du processus trainer
%% Inputs : liste des PID des entrée du résaux de neurone
%% Training_list : liste de couple entrée sortie à présenter au résaux de neurone (les valeurs sont présenté sous forme de liste, trié dans l'ordre des rang des entrées/sorties
%% Training_constants : {Threshold, Primus_F, Speed}
%%                  Threshold : marge d'erreur acceptable par l'entraineur
%%                  Primus_F : dérivée de la fonction d'activation du neuronne
%%                  Speed : vitesse d'aprentissage
%%                  Max : nombre d'itération maximal

train(Trainer_pid, Inputs, Training_list, Training_constants) ->
    train(Trainer_pid, Inputs, Training_constants, Training_list, [], [], 0).

train(_, _, _, [], [], _, _) ->
    ok;
train(_, _, {_, _, _, I}, _, _, _, I) ->
    max;

train(Trainer_pid, Inputs, Training_constants, [], False_list, True_list, I) ->
    Training_list = utils:shuffle(False_list ++  True_list),
    train(Trainer_pid, Inputs, Training_constants, Training_list, [], [], I);

train(Trainer_pid, Inputs, Training_constants, [Training_values | Training_list], False_list, True_list, I ) ->
    {Training_inputs, Training_outputs} = Training_values,

    New_training_constants = add_training_output(Training_constants, Training_outputs),
    Trainer_pid ! {train, self(), New_training_constants},

    F = fun ({Input, Val}, _) ->
		Input ! {input, Val},
		ok
	end,
    lists:foldl(F, ok, lists:zip(Inputs, Training_inputs)),

    receive
	ok -> 
	    train(Trainer_pid, Inputs, Training_constants, Training_list, False_list, [Training_values | True_list], I + 1);
	updated ->
    	    train(Trainer_pid, Inputs, Training_constants, Training_list, [Training_values | False_list], True_list, I + 1)
    end.

add_training_output (Training_constant, Training_output) ->
    {A, B, C, _} = Training_constant,
    {A, B, C, Training_output}.


%%fonction pour le processus d'entrainement.
%% Chaque neurone + les input doivent envoyer leur résultat au trainer
%% Network_value : {Nb_neuron, Nb_layer, Weights}
%%      Nb_neuron : nombre de neurone total dans le résaux (en comptant les entrées)
%%      Nb_layer : liste du nombre de neurone par couche  (attention la couche de sortie ne compte pas dedans)
%%      Weights : table de hachage contenant les poids. les clés sont un couple (layer, rak)
%% Quand il a reçu un jeu de test, attend les sortie des différent neurone puis rétropropage les erreur
%% puis il se remet en attente d'un jeu de test

trainer(Network_value) ->
    receive 
	{train, From, Training_constants} ->
	    %% {_, _, _, Training_outputs} = Training_constants,
	    %% io:format("training outputs : ~p", [Training_outputs]),
	    trainer(From, Training_constants, gb_trees:empty(), Network_value, 0) 
    end.


trainer(Output, Training_constants, Outputs, Network_value, Outputs_received) ->
    case Network_value of
	{Outputs_received, _, _} ->
	    case backpropagation(Training_constants, Outputs, Network_value) of
		{updated, New_network_value} -> 
		    Output ! updated,
		    trainer(merge_network_values(Network_value, New_network_value));
		_ -> Output ! ok,
		    trainer(Network_value)
	    end;
	_ ->
	    receive
		{done, Result, {Neuron, Layer, Rank}} ->
		    %% io:format("trainer receive output from ~p,~p~n", [Layer, Rank]),
		    New_outputs = gb_trees:insert({Layer, Rank}, {Neuron, Result}, Outputs),
		    trainer(Output, Training_constants, New_outputs, Network_value, Outputs_received + 1)
	    end
    end.

%%fusione les ancienne valeur du resaux avec les nouvelles
merge_network_values(Old, New) ->
    {N, Layer, _} = Old,
    {_, _, W} = New,
    {N, Layer, W}.

%% compare les sortie de chaque neurone au jeu de test et met a jours les poids de chaque neurone.
%% renvoi updated si les poids ont été mis a jour,
%% ok sinon
%% Network_value : {Nb_neuron, Nb_layer, Weights}
%%      Nb_layer : liste du nombre de neurone par couche (attention la couche de sortie ne compte pas dedans)
%%      Weights : table de hachage contenant les poids. les clés sont un couple (layer, rak)
%% Training_constants : {Threshold, Primus_F, Speed}
%%                  Threshold : marge d'erreur acceptable par l'entraineur
%%                  Primus_F : dérivée de la fonction d'activation du neuronne
%%                  Speed : vitesse d'aprentissage
%%                  Training_output : liste des valeur attendut en sortie du resaux
%% Outputs : un gb_trees avec un couple (couche/rang) en clef et un couple (PID, Valeur) comme valeur, les valeur d'entrées du resaux sont aussi stocké à la couche la plus grand
backpropagation(Training_constants, Outputs, Network_value) ->
    {Threshold, _, Speed, Training_outputs} = Training_constants,
    %% fonction pour ne retenir que les valeur calculé par la dernière couche
    F = fun({{0, _}, {_, Val}}, Acc) -> [Val | Acc];
	   (_, Acc) -> Acc end,
    Real_values = lists:reverse(lists:foldl(F, [], gb_trees:to_list(Outputs))),
    case compute_error(Real_values, Training_outputs, Threshold) of
	ok -> ok;
	{update, Errors} ->
		backpropagation (Speed, Outputs, Network_value, Errors, 0, 0, gb_trees:empty())
    end.

backpropagation(Speed, Outputs, Network_value, Errors, Layer, Rank, Gradients) ->
    case Network_value of
	{_, [], _} -> {updated, Network_value};
	{N, [Rank | Other_layer], W} -> 
	    backpropagation(Speed, Outputs, {N, Other_layer, W}, Errors, Layer + 1, 0, Gradients);
	{ Nb_neuron, L, Weights} -> 
	    Weight = gb_trees:get({Layer, Rank}, Weights),
	    %% Weight = cube:get_as_list(Layer, Rank, Weights),
	    {Neuron, Output} = gb_trees:get({Layer, Rank}, Outputs),
	    Gradient = 
		case Layer of
		    0 ->
			Error = gb_trees:get(Rank, Errors),
			compute_gradient_output(Error, Output);
		    _ ->
			Previous_gradients = get_previous_gradient(Layer, Gradients),
			compute_gradient(Output, Weights, Previous_gradients, Layer, Rank)
		end,
	    F = fun(W, {I, Acc}) -> 
			Previous_value = 
			    case I of 
				0 -> 1;
				_ -> {_, V} = gb_trees:get({Layer + 1, I-1}, Outputs),
				     V
			    end,

			Nw = W + Speed * Gradient * Previous_value,
			{I + 1, [Nw | Acc]}
		end,
	    {_, New_Weight} = lists:foldl(F, {0, []}, Weight),
	    New_weights_tree = gb_trees:update({Layer, Rank}, New_Weight, Weights),
	    %% New_weights_cube = cube:update_line_from_list(Layer, Rank, New_Weight, Weights),
	    New_gradient_tree = gb_trees:insert({Layer, Rank}, Gradient, Gradients),
	    %% io:format(" update neuron ~p,~p with ~p~n", [Layer, Rank, New_Weight]),
	    Neuron ! {update, New_Weight},
	    New_network_value = {Nb_neuron, L, New_weights_tree},

	    backpropagation(Speed, Outputs, New_network_value, Errors, Layer, Rank + 1, New_gradient_tree)
    end.


%%Renvoi la liste des gradient des sorties conectées à un neurone de la couche Layer
%% Layer : la couche du neurone courante
%% gradients : gb_tree contenant les gradient de chaque neurone avec comme cléf un couple (Layer,Rank)
get_previous_gradient(Layer, Gradients) ->
    F = fun ({{L, Rank}, G }, Acc) when L =:= Layer - 1 -> [{Rank, G} | Acc] ;
	    (_, Acc) -> Acc 
	end,
    F2 = fun({A, _}, {B, _}) -> A =< B end,
    F3 = fun ({_, G}) -> G end,
    L1 = lists:foldl(F, [], gb_trees:to_list(Gradients)),
    L2 = lists:sort(F2, L1), 
    lists:map(F3, L2).


%% calcule le gradient pour les neurone de la couche de sortie
%% Error : l'erreur calculé pour le neurone
%% Output : sortie du neurone
compute_gradient_output(Error, Output) ->
    Error * Output * (1 - Output).


%% Calcule le gradient des couche masqué
%% Output : valeur de sortie du neurone
%% Weights : liste des poids appliqué aux entrée (trié dans l'ordre croissant)
%% Gradients : liste des gradients des sorties du neurone (trié dans l'ordre croissant)
compute_gradient(Output, Weights, Gradients, Layer, Rank) ->
    F = fun ({{I,_}, Val}, Acc) when (I =:= Layer - 1)  ->
		Array = array:from_list(Val),
		[array:get(Rank, Array) | Acc];
	    (_, Acc) -> Acc
	end,
    Weights_list = lists:reverse( lists:foldl(F, [], gb_trees:to_list(Weights))),
    F2 = fun ({W, G}, Acc) -> (W * G) + Acc end,
    %% io:format("~p, ~p~n", [Weights_list, Gradients]),
    Sum = lists:foldl(F2, 0, lists:zip(Weights_list, Gradients)),
    Output * (1 - Output) * Sum.


%% renvoi un gb_tree contenant la liste des erreur (ordonné par leur rang) si l'erreur moyenne est superieur au seuil sinon renvoi ok
compute_error(Real_values, Value, Threshold) ->
    F = fun ({A, B}, Acc) -> [A - B | Acc] end,
    F2 = fun (A, Acc) -> A + Acc end,
    Result = lists:reverse(lists:foldl(F, [], lists:zip(Value, Real_values))),
    Average = lists:foldl(F2, 0, Result) / length(Result),
    %% io:format("Average of error : ~p~n, ", [abs(Average)]),
    case math:pow(Average, 2) of
	 Quad when Quad > Threshold -> 
	    F3 = fun(A, {I, List}) -> {I + 1, [{I, A} | List]} end,
	    {_, List} = lists:foldl(F3, {0, []}, Result),
	    Tree = gb_trees:from_orddict( lists:reverse(List) ),
	    %% io:format("ready for update~n, "),
	    {update, Tree};
	_ -> ok
    end.
					   
