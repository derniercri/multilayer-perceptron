-module(neuron).
-compile(export_all).


%% Fonction fournissant à un processus une sortie générique affichant le résultat envoyé par un neuronne
output_progress() ->
    output(0).

output(N) ->
    receive
	{done, _, _} -> 
	    if
		(N rem 100000) =:= 0->
		    io:format("~p steps~n",[N]),
		    output(N + 1);
		true ->
		    output(N + 1)
	    end;
	_ ->  
	    output(N + 1)
    end.

output() ->
    receive
        {done, Result, {_, Layer, Rank}} -> 
            io:format(
              "couche : ~p, neurone ~p, resultat : ~p~n", 
              [Layer, Rank, Result]
             ),
            output();
        _ -> output()
    end.


%% Connecte la sortie Output à tout les neurones de la couche Layer
%% Output : PID de la sortie à connecter à chaque neurones
%% Layer : liste des PID de tout les neurones à connecté
connect_output(Output, Layer) ->
    F = fun(PID) -> PID ! {connect_output, Output} end,
    lists:foreach(F, Layer).
	


%% Fonction fournissant à un processus une entrée générique permettant
%% d'envoyer des valeur à un ou plusieurs neuronne.
%% Rank est le rang de l'entrée, elle est transmise aux neuronnes conectés pour qu'ils sachent d'où vient la valeur qu'ils reçoivent
%% Outputs est la liste des Neurones auxquel le processus doit envoyé un
%%  message quand il reçoit une valeur. C'est une liste de PID

input(Rank, Outputs) ->
    receive
        {input, Value} ->
            send_msg_to_list(Outputs, {done, Value, {self(), -1, Rank}}),
            input(Rank, Outputs);
        _ -> input(Rank, Outputs)
    end.

input(Layer, Rank, Outputs) ->
    receive
        {input, Value} ->
            send_msg_to_list(Outputs, {done, Value, {self(), Layer, Rank}}),
            input(Layer, Rank, Outputs);
	{connect_output, From} -> 
	    input(Layer, Rank, [From | Outputs]);
        _ ->
	    input(Layer, Rank, Outputs)
    end.



%% créer le resaux
%% renvoi une triplet {Network, Input_list, Output_list, Network_length}
%% Network : une matrice contenant un couple {PID, nb_inputs} représentant un neuron
%% Input_list : liste des PID des entrées du resaux
%% Output_list : liste des PID de la couche de sortie du resaux
%% Network_size : liste des taille de chaque couches

%% Arguments
%% Layer_values : liste de triplet {N, P, F} représentant une couche avec :
%%     N : nombre de neurone sur la couche
%%     P : nombre d'entrée de la couche
%%     F : fonction d'activation de la couche
%% Nb_inputs : nombre d'entrée du résaux
%% Nb_layer : nombre de couche
make_network(Layer_values, Nb_inputs, Nb_layer) ->
    %% fonction parcourant Layer_values pour initialiser chaque couche
    F = fun({Nb_neuron, Nb_previous, Fun}, {Network, I, Old_layer}) ->
		Layer = make_layer(I, Old_layer, Nb_neuron, Nb_previous, Fun),
		F2 = fun(PID, {J, Layer_array}) ->
			     New_array = array:set(J, {PID, Nb_previous + 1}, Layer_array),
			     {J + 1, New_array} 
		     end,
		{_, Layer_array} = lists:foldl(F2, {0, array:new(Nb_neuron)}, Layer),
		{array:set(I, Layer_array, Network), I + 1, Layer}
	end,
    
    %% Création du resaux
    {Network, _, Last_layer} = lists:foldl(F, {array:new(Nb_layer), 0, []}, Layer_values),
    %% Création des entrées
    Input_list = make_inputs(Nb_inputs, Nb_layer, Last_layer),

    %% Création, de la liste des sorties
    Output_array = array:get(0, Network),
    F2 = fun(_, {PID, _}, List) -> [PID | List] end,
    Output_list = lists:reverse(array:foldl(F2, [], Output_array)),

    %% Création de la liste des tailles de chaque couche
    {Size, _, _} = lists:unzip3(Layer_values),
    Network_size = Size ++ [Nb_inputs],

    {Network, Input_list, Output_list, Network_size}.


%% créer N input d'indice Layer connecté à chaque PID contenue dans PID_list
make_inputs(N, Layer_rank, PID_list) ->
    make_inputs(N, Layer_rank, PID_list, []).


make_inputs(0, _, _, Acc) -> Acc;

make_inputs(N, Layer_rank, PID_list, Acc) -> 
    Input = spawn( fun() -> neuron:input(Layer_rank, N, PID_list) end),
    make_inputs(N - 1, Layer_rank, PID_list, [Input | Acc]).
    
    
					      


%% crée une couche de neurone dont les poids et les biais sont initialisés à 0
%% Layer_rank : indice de la couche
%% Outputs : liste des sortie de la couche
%% Nb_neuron : nombre de neurone sur la couche
%% Nb_inputs : nombre de neurone sur la couche précédente
%% Fun : fonction d'activation de la couche
make_layer(Layer_rank, Outputs, Nb_neuron, Nb_inputs, Fun) ->
    Value = {Nb_inputs, [], 0, Fun},
    init_layer(Layer_rank, Outputs, Nb_neuron, Value).
    
%% Fonction auxiliaire de make_layer
init_layer(Layer_rank, Outputs, N, Value) ->
    init_layer(Layer_rank, Outputs, N, Value, []).

init_layer(_, _, 0, _, Layer) ->
    Layer;

init_layer(Layer_rank, Outputs, N, Value, Layer) ->
    Neuron = init_neuron(Outputs, Layer_rank, N, Value),
    init_layer(Layer_rank, Outputs, N - 1, Value, [Neuron | Layer]).
						  


%% Créer une couche de neurone en initialisant les poids et les biais de chaque neurones et renvoie la liste de leurs PID's
%% Layer_rank : indice de la couche
%% Outputs : liste des sorties à relier à chaque neurone
%% Neuron_values : valeurs de chaque neurone

make_layer_hard(Layer_Rank, Outputs, Neuron_values) ->
    F = fun (Neuron_value, {Rank, Acc}) ->
                Neuron = init_neuron(Outputs, Layer_Rank, Rank, Neuron_value),
                {Rank + 1, [Neuron | Acc]}
        end,
    {_, L} = lists:foldl(F, {1, []}, Neuron_values),
    lists:reverse(L).


%% Crée un neurone et le charge dans un processus.
%% Outputs est la liste des sorties auxquel le neurone doit envoyer 
% le résultat de son calcul une fois effectué;
%% Layer est l'indice de la couche du neurone créé.
%% Rank est l'indice du neurone dans la couche
%% Values est un tuple contenant les valeur du neurone (voir README)
init_neuron(Outputs, Layer, Rank, Values) ->
    {N, W, B, F} = Values,
    New_values = {N, [B | W], F},
    %% insertion de l'entrée du biais
    Inputs = gb_trees:insert(0, 1, gb_trees:empty()),
    Env = {Outputs, Inputs, Layer, Rank, New_values},
    spawn(fun() -> run_neuron(Env, 0) end).


%% Met le neurone en attente de stimulation, quand suffisamment d'entrées ont 
%% stimulé le neurone, envoie à toutes les sorties le résultat du calcul.

run_neuron(
  {
    Outputs, 
    Inputs, 
    Layer, 
    Rank, 
    {Nb_inputs, Weights, F}
  }, Nb_inputs
 ) ->
    Inputs_list = utils:gb_trees_to_sorted_list(Inputs),
    Result = compute({Weights, F}, Inputs_list),
    Msg = {done, Result, {self(), Layer, Rank}},
    %% io:format("result of neuron ~p,~p = ~p~n",[Layer, Rank, Result]), 
    ok = send_msg_to_list(Outputs, Msg),
    New_inputs = gb_trees:insert(0, 1, gb_trees:empty()),
    run_neuron({Outputs, New_inputs, Layer, Rank, {Nb_inputs, Weights, F}}, 0);

run_neuron(Env, Nb_inputs) ->
    {Outputs, Inputs, Layer, Rank, Values} = Env,    
    receive
        %% Recevoir une entrée d'un autre neuronne
        {done, Result, {_, _, Rank_N}} -> 
            %% io:format("input in neuron ~p,~p : ~p~n", [Layer, Rank,Result]),
            run_neuron(
              {
                Outputs, 
                gb_trees:insert(Rank_N, Result, Inputs), 
                Layer, 
                Rank, 
                Values
              }, Nb_inputs + 1);
        
        %% Met a jour le neurone, utilisé pendant la phase d'apprentissage
        {update, New_weights} ->
            New_values = update_weights(Values, New_weights),
            run_neuron({Outputs, Inputs, Layer, Rank, New_values}, Nb_inputs);
        {get_weight, From} ->
            Weights = get_weights(Values),
            From ! {give_weight, Weights},
	    run_neuron(Env, Nb_inputs);
	{connect_output, PID} ->
	    New_env = {[PID | Outputs], Inputs, Layer, Rank, Values},
	    run_neuron(New_env, Nb_inputs)
    end.

%% extrait le poids du tuple contenant les valeurs du neurone
get_weights({_, Weights, _}) -> Weights.

%% Met à jour le poids du tuple contenant les valeurs du neurone
update_weights(Values, New_weights) ->
    {Nb_inputs, _, F} = Values,
    {Nb_inputs, New_weights, F}.


%% calcule le résultat d'un neurone à partir d'un jeu d'entrées.
compute({W,F}, Input) ->
    io:format("~p  ~p~n", [Input, W]),
    Fun = fun ({Val, Weight}, Acc) -> Val * Weight + Acc end,
    L = lists:zip(Input, W),
    Val = lists:foldl(Fun, 0, L),
    F(Val).


%% envoi un message à une liste des PID
send_msg_to_list(Pids, Msg) ->
    lists:foreach(fun(Pid) -> Pid ! Msg end, Pids).
