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

    %% création des neurones
    Weight_null = [1, 1],
    F = fun (X) -> utils:sigmoid(X) end,
        
    N1 = {2, Weight_null, 0, F},
    N2 = {2, Weight_null, 0, F},
    N3 = {2, Weight_null, 0, F},
    
    Output = spawn(fun() -> neuron:output_progress() end),

    C1 = neuron:make_layer(0, [Output], [N1]),
    C2 = neuron:make_layer(1,  C1, [N2, N3]),

    %% création du resaux 
    L1 = array:from_list([{hd(C1), 3}]),
    L2 = array:from_list([{hd(C2), 3},
			  {hd(tl(C2)), 3}]),
    Network = array:from_list([L1, L2]),

    %% init_trainer
    N_inputs = 5,
    Network_size = [1, 2, 2],
    Self = self(),
    Trainer = spawn(fun() -> trainer:train_init(Network, N_inputs, Network_size, Self) end),

    Input1 = spawn(fun () -> neuron:input(2, 1, [Trainer | C2]) end),
    Input2 = spawn(fun () -> neuron:input(2, 2, [Trainer | C2]) end),
    
    %%trainer constant
    Threshold = 0,
    Primus_F = F,
    Speed = 0.01,
    Max_iter = 1000,

    Training_list = [ {[1,0], [1]}, {[0,1], [1]}, {[0,0], [0]}, {[1,1], [0]}],
    
    io:format("init trainer~n"),
    receive
	ok ->
	    io:format("launching test~n", []),
	    trainer:trainer(Trainer, [Input1, Input2], Training_list, {Threshold, Primus_F, Speed, Max_iter}),
	    hd(C1) ! {connect_output, spawn(fun () -> neuron:output() end)},
	    ok = test_neuron([Input1, Input2], [0, 1], 1),
	    ok = test_neuron([Input1, Input2], [1, 1], 0),
	    ok = test_neuron([Input1, Input2], [1, 0], 1),
	    ok = test_neuron([Input1, Input2], [0, 0], 0)
    end.

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
	    
