-module(trainer_test).
-include_lib("eunit/include/eunit.hrl").


%%compute_error
compute_error_test() ->
    {update, Tree_test} = trainer:compute_error([1,2,3], [0,1,2], 0),
    Tree = gb_trees:from_orddict([{0, -1}, {1, -1}, {2, -1}]),
    ?assertEqual(Tree_test, Tree).

compute_error2_test() ->
    {update, Tree_test} = trainer:compute_error([1], [0], 0),
    Tree = gb_trees:from_orddict([{0, -1}]),
    ?assertEqual(Tree_test, Tree).

compute_error3_test() ->
    ?assertEqual( trainer:compute_error([1, 2, 3], [1, 2, 3], 0), ok).

%% lauch_trainer
launch_test() ->
    Weight_n1 = [-0.1, 0.5],
    Weight_n2 = [-0.2, -0.7],
    Weight_n3 = [0, 0.4],

    B1 = 0.1,
    B2 = 0.9,
    B3 = -0.4,

    F = fun (X) -> utils:sigmoid(X) end,
    
    Weights_tree = gb_trees:from_orddict([{{0,0}, [B1 | Weight_n1]}, {{1,0}, [B2 | Weight_n2]}, {{1,1}, [B3 | Weight_n3]} ]),

    %% Weights_list = [B1 | Weight_n1] ++ [0,0,0] ++ [B2 | Weight_n2] ++ [B3 | Weight_n3],
    %% Weights_cube = cube:from_list(Weights_list, 2, 2, 3),
    
    Nb_layer = [1, 2],
    Nb_neuron = 5,
    
    Trainer = spawn(fun() -> trainer:trainer({Nb_neuron, Nb_layer, Weights_tree}) end),

    N1 = {2, Weight_n1, B1, F},
    N2 = {2, Weight_n2, B2, F},
    N3 = {2, Weight_n3, B3, F},
    
    Output = spawn(fun() -> neuron:output() end),

    C1 = neuron:make_layer(0, [Output, Trainer], [N1]),
    C2 = neuron:make_layer(1, [Trainer | C1], [N2, N3]),

    Input1 = spawn(fun () -> neuron:input(2, 0, [Trainer | C2]) end),
    Input2 = spawn(fun () -> neuron:input(2, 1, [Trainer | C2]) end),
    
    %%trainer constant
    Threshold = 0,
    Primus_F = F,
    Speed = 2,
    Max_iter = 100000,
    
    Training_list = [ {[1,0], [1]}, {[0,1], [1]}, {[0,0], [0]}, {[1,1], [0]}],
    
    io:format("launching test~n", []),
    trainer:train(Trainer, [Input1, Input2], Training_list, {Threshold, Primus_F, Speed, Max_iter}),
    {Input1, Input2}.
