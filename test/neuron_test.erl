-module(neuron_test).
-include_lib("eunit/include/eunit.hrl").

test_neuron(Inputs, Values, Output) ->
    F = fun ({Input, Val}) -> Input ! {input, Val} end,
    ok = lists:foreach(F, lists:zip(Inputs, Values)),
    receive
	{done, Val, _} ->
	    ?assertEqual(Val, Output),
	    ok;
	_ ->
	    ?assertEqual(1, 2),
	    none
    end.

xor_test() ->
    L1 = [{2, [1, 1], 1.5, utils:seuil(0)}],
    L2 = [{2, [2, 2], 1, utils:seuil(0)},
	  {2, [-1, -1], -1.5, utils:seuil(0)}],
    C1 = neuron:make_layer(0, [self()], L1),
    C2 = neuron:make_layer(1, C1, L2),
    Input1 = spawn(fun () -> neuron:input(0, C2) end),
    Input2 = spawn(fun () -> neuron:input(1, C2) end),
    ok = test_neuron([Input1, Input2], [0, 1], 1),
    ok = test_neuron([Input1, Input2], [1, 1], -1),
    ok = test_neuron([Input1, Input2], [1, 0], 1),
    ok = test_neuron([Input1, Input2], [0, 0], -1).
