-module(neuron).
-compile(export_all).


%% Fonction fournissant à un processus une sortie générique affichant le résultat envoyé par un neuronne
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
        _ -> input(Layer, Rank, Outputs)
    end.
	    
%% Créer une couche de neurone et renvoie la liste de leurs PID's
%% Layer_rank : indice de la couche
%% Outputs : liste des sorties à relier à chaque neurone
%% Neuron_values : valeurs de chaque neurone

make_layer(Layer_Rank, Outputs, Neuron_values) ->
    F = fun (Neuron_value, {Rank, Acc}) ->
                Neuron = init_neuron(Outputs, Layer_Rank, Rank, Neuron_value),
                {Rank + 1, [Neuron | Acc]}
        end,
    {_, L} = lists:foldl(F, {0, []}, Neuron_values),
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
    Inputs = gb_trees:insert(-1, 1, gb_trees:empty()),
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
    New_inputs = gb_trees:insert(-1, -1, gb_trees:empty()),
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
            From ! {give_weight, Weights}
                
    end.

%% extrait le poids du tuple contenant les valeurs du neurone
get_weights({_, Weights, _}) -> Weights.

%% Met à jour le poids du tuple contenant les valeurs du neurone
update_weights(Values, New_weights) ->
    {Nb_inputs, _, F} = Values,
    {Nb_inputs, New_weights, F}.


%% calcule le résultat d'un neurone à partir d'un jeu d'entrées.
compute({W,F}, Input) ->
    Fun = fun ({Val, Weight}, Acc) -> Val * Weight + Acc end,
    L = lists:zip(Input, W),
    Val = lists:foldl(Fun, 0, L),
    F(Val).


%% envoi un message à une liste des PID
send_msg_to_list(Pids, Msg) ->
    lists:foreach(fun(Pid) -> Pid ! Msg end, Pids).
