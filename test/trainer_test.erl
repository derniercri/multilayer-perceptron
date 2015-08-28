-module(trainer_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%% fonction permetant de tester les sortie d'un résaux de neuron
test_neuron(Inputs, Values, Output) ->
    F = fun ({Input, Val}) -> Input ! {input, Val} end,
    ok = lists:foreach(F, lists:zip(Inputs, Values)),
    receive
	{done, Val, _} ->
	    ?assertEqual(lissage(Val), Output),
	    ok;
	_ ->
	    ?assertEqual(1, 2),
	    none
    end.     

lissage(N) when N >= 0.9 -> 1;
lissage(N) when N =< 0.1 -> 0;
lissage(N) -> N.

launch_test() ->

    %% création du resaux 
    F = fun (X) -> utils:sigmoid(X) end,
    L = [{1, 2, F},
	 {2, 2, F}],
    {Network, Input_list, Output_list, Network_size} = neuron:make_network(L, 2, 2),

    %% Initialisation du superviseur
    Trainer = trainer:init(Network, Network_size, Input_list),
    
    %%trainer constant
    Threshold = 0,
    Primus_F = F,
    Speed = 1,
    Max_iter = 10000,

    Training_list = [ {[1,0], [1]}, {[0,1], [1]}, {[0,0], [0]}, {[1,1], [0]}],

    %% Lancement de l'apprentissage
    trainer:launch(Trainer, Input_list, Training_list, {Threshold, Primus_F, Speed, Max_iter}),

    %% Test des résultat
    neuron:connect_output(self(), Output_list),
    ok = test_neuron(Input_list, [0, 1], 1),
    ok = test_neuron(Input_list, [1, 1], 0),
    ok = test_neuron(Input_list, [1, 0], 1),
    ok = test_neuron(Input_list, [0, 0], 0).
