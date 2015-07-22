-module(matrix_test).
-include_lib("eunit/include/eunit.hrl").

size_test() ->
    C1 = matrix:new(1, 2),
    ?assertEqual(matrix:size(C1), {1, 2}).

empty_size_test() ->
    C2 = matrix:new(1, 2),
    C3 = matrix:new(1, 0),
    C4 = matrix:new(0, 0),
    ?assertEqual(matrix:size(C2), {1, 2}),
    ?assertEqual(matrix:size(C3), {1, 0}),
    ?assertEqual(matrix:size(C4), {0, 0}).

get_test() ->
    C1 = matrix:new(1, 2),
    C2 = matrix:set(0, 1, 333, C1),
    ?assertEqual(matrix:get(0, 1, C2), 333).

foldl_sum_test() ->
    Matrix = matrix:new(1, 2, {default, 1}),
    F = fun ( _, _, Val, Acc) -> Acc + Val end,
    Acc = matrix:foldl(F, 0, Matrix),
    ?assertEqual(Acc, 2).

map_test() ->
    Matrix = matrix:new(1, 2, {default, 0}),
    F = fun(I, J, Val) -> I + J + Val end,
    Matrix2 = matrix:map(F, Matrix),
    ?assertEqual(matrix:get(0, 0, Matrix2), 0),
    ?assertEqual(matrix:get(0, 1, Matrix2), 1).

    


			     
		
		    
		    
    
		     
