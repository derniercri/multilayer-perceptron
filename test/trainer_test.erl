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



