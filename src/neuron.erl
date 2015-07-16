-module(neuron).
-compile(export_all).


    


%% Fonction fournissant à un processus une sortie générique affichant le résultat envoyé par un neuronne
output() ->
    receive
	{done, Result, {_, Layer, Rank}} -> 
	    io:format(" couche : ~p, neurone ~p, resultat : ~p~n", [Layer, Rank, Result]),
	    output();
	_ ->  
	    output()
    end.


%% Fonciton fournissant à un processus une entrée générique permétant d'envoyer des valeur à un ou plusieurs neuronne.
%% Rank est le rang de l'entrée, elle est transmise aux neuronne conecté pour qu'ils sache d'ou vient la valeur qu'ils reçoivent
%% Outputs est la liste des Neurone auquel le processus doit envoyé un message quand il reçoit une valeur. C'est une liste de PID
%%

input(Rank, Outputs) ->
    receive
	{input, Value} ->
	    send_msg_to_list(Outputs, {done, Value, {self(), -1, Rank}}),
	    input(Rank, Outputs);
	_ -> 
	    input(Rank, Outputs)
    end.

	    
%%Créer une couche de neurone et renvoi la liste de leur PID
%% Layer_rank : indice de la couche
%% Outputs : liste des sortie à relié à chaque neurone
%% Neuron_values : valeurs de chaque neurone

make_layer(Layer_Rank, Outputs, Neuron_values) ->
    F = fun (Neuron_value, {Rank, Acc}) ->
		Neuron = init_neuron(Outputs, Layer_Rank, Rank, Neuron_value),
		{Rank + 1, [Neuron | Acc]}
	end,
    {_, L} = lists:foldl(F, {0, []}, Neuron_values),
    lists:reverse(L).


%% Crée un neuronne et le charge dans un processus.
%% Outputs est la liste des sortie auquel le neuronne doit envoyer le résultat de son calcul une fois effectué
%% Layer est l'indice de la couche du neurone créé.
%% Rank est l'indice du neurone dans la couche
%% Values est un tuple contenant les valeur du neurone (voir README)
init_neuron(Outputs, Layer, Rank, Values) ->
    Env = {Outputs, gb_trees:empty(), Layer, Rank, Values},
    spawn(fun() -> run_neuron(Env, 0) end).


%% met le neuronne en attente de stimulation, quand suffisament d'entrée ont stimulé le neuronne, envois a toutes les sortie le résultat du calcul.

run_neuron({Outputs, Inputs, Layer, Rank, {Nb_inputs, Weights, B, F}}, Nb_inputs) ->
    Inputs_list = gb_trees_to_sorted_list(Inputs),
    Result = compute({Weights, B, F}, Inputs_list),
    Msg = {done, Result, {self(), Layer, Rank}},
    io:format("result : ~p~n",[Result]), 
    ok = send_msg_to_list(Outputs, Msg),
    run_neuron({Outputs, gb_trees:empty(), Layer, Rank, {Nb_inputs, Weights, B, F}}, 0);

run_neuron(Env, Nb_inputs) ->
    {Outputs, Inputs, Layer, Rank, Values} = Env,    
    receive
	%% Recevoir une entrée d'un autre neuronne
	{done, Result, {_, _, Rank_N}} -> 
	    io:format("input in neuron ~p,~p : ~p~n", [Layer, Rank,Result]),
	    run_neuron({Outputs, gb_trees:insert(Rank_N, Result, Inputs), Layer, Rank, Values}, Nb_inputs + 1);
	%% Met a jour le neurone, utilisé pendant la phase d'apprentissage
	{update, New_weights} ->
	    io:format("update neuron ~p,~p with value : ~p~n", [Layer, Rank, New_weights]),
	    New_values = update_weights(Values, New_weights),
	    run_neuron({Outputs, Inputs, Rank, New_values}, Nb_inputs);
	{get_weight, From} ->
	    Weights = get_weights(Values),
	    From ! {give_weight, Weights}
		
    end.

get_weights(Values) ->
    {_, Weights,_, _} = Values,
    Weights.

update_weights(Values, New_weights) ->
    {Nb_inputs, _, B, F} = Values,
    {Nb_inputs, New_weights, B, F}.

%% convertie un gb_tree en liste triée
gb_trees_to_sorted_list(Gb_Trees) ->
    List = gb_trees:to_list(Gb_Trees),
    F = fun({R1, _}, {R2, _}) ->  R1 =< R2 end,
    Sorted_list = lists:sort(F, List),
    F2 = fun({_, V}) -> V end,
    lists:map(F2, Sorted_list).


%% calcule le résultat d'un neurone à partir d'un jeu d'entrée.
compute({W,B,F}, Input) ->
    Fun = fun ({Val, Weight}, Acc) ->
		  Val * Weight + Acc
	  end,
    L = lists:zip(Input, W),
    Val = lists:foldl(Fun, 0, L),
    F(Val - B).


%% envoi un message à une liste des PID
send_msg_to_list([Pid | Outputs], Msg) ->
    Pid ! Msg,
    send_msg_to_list(Outputs, Msg);
send_msg_to_list([], _) ->
    ok.
