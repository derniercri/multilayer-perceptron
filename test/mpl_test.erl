-module(mpl_test).
-include_lib("eunit/include/eunit.hrl").

xor_interface_test() ->
    io:format("launched~n"),
    F = fun (X) -> utils:sigmoid(X) end,
    L = [{1, 2, F},
	 {2, 2, F}],
    Threshold = 0,
    Primus_F = F,
    Speed = 1,
    Max_iter = 10000,
    Training_constant =  {Threshold, Primus_F, Speed, Max_iter},
    Training_values = [ {[1,0], [1]}, {[0,1], [1]}, {[0,0], [0]}, {[1,1], [0]}],

    mpl:start_link(L, 2, 2),
    mpl:train(Training_values, Training_constant),
    wait_training(),
    test_call([0, 1], [1]),
    test_call([1, 1], [0]),
    test_call([1, 0], [1]),
    test_call([0, 0], [0]).
    

lissage(N) when N >= 0.9 -> 1;
lissage(N) when N =< 0.1 -> 0;
lissage(N) -> N.


wait_training() ->
    receive
	{From_, {training_done, Result}} -> ok;
	Other -> wait_training()
    end.

test_call(Input, Output_ini) ->
    mpl:compute(Input),
    receive
	{From_, {compute_done, Outputs}} ->
	    Outputs_lissed = lists:map(fun lissage/1, Outputs),
	    ?assertEqual(Output_ini, Outputs_lissed);
	Other ->
	    test_call(Input, Output_ini)
    end.
