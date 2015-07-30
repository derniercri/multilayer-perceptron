-module(trainer_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


%% %%compute_error
%% compute_error_test() ->
%%     {update, Tree_test} = trainer:compute_error([1,2,3], [0,1,2], 0),
%%     Tree = gb_trees:from_orddict([{0, -1}, {1, -1}, {2, -1}]),
%%     ?assertEqual(Tree_test, Tree).

%% compute_error2_test() ->
%%     {update, Tree_test} = trainer:compute_error([1], [0], 0),
%%     Tree = gb_trees:from_orddict([{0, -1}]),
%%     ?assertEqual(Tree_test, Tree).

%% compute_error3_test() ->
%%     ?assertEqual( trainer:compute_error([1, 2, 3], [1, 2, 3], 0), ok).

%% launch_trainer
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
    trainer:trainer(Trainer, Input_list, Training_list, {Threshold, Primus_F, Speed, Max_iter}),
    neuron:connect_output(self(), Output_list),
    ok = test_neuron(Input_list, [0, 1], 1),
    ok = test_neuron(Input_list, [1, 1], 0),
    ok = test_neuron(Input_list, [1, 0], 1),
    ok = test_neuron(Input_list, [0, 0], 0).

lissage(N) when N >= 0.9 -> 1;
lissage(N) when N =< 0.1 -> 0;
lissage(N) -> N.
			   
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

get_weight(N) ->
    N ! {get_weight, self()},
    receive 
	{give_weight, Weights} -> Weights;
	_ -> error
    end.
	    
