-module(cube_test).
-include_lib("eunit/include/eunit.hrl").

size_test() ->
    C1 = cube:new(1, 2, 3),
    ?assertEqual(cube:size(C1), {1, 2, 3}).

empty_size_test() ->
    C2 = cube:new(1, 2, 0),
    C3 = cube:new(1, 0, 0),
    C4 = cube:new(0, 0, 0),
    ?assertEqual(cube:size(C2), {1, 2, 0}),
    ?assertEqual(cube:size(C3), {1, 0, 0}),
    ?assertEqual(cube:size(C4), {0, 0, 0}).

get_test() ->
    C1 = cube:new(1, 2, 3),
    C2 = cube:set(0, 1, 2, 333, C1),
    ?assertEqual(cube:get(0, 1, 2, C2), 333).

foldl_sum_test() ->
    Cube = cube:new(1, 2, 3, {default, 1}),
    F = fun (_, _, _, Val, Acc) -> Acc + Val end,
    Acc = cube:foldl(F, 0, Cube),
    ?assertEqual(Acc, 6).

map_test() ->
    Cube = cube:new(1, 2, 3, {default, 0}),
    F = fun(I, J, K, Val) -> I + J + K + Val end,
    Cube2 = cube:map(F, Cube),
    ?assertEqual(cube:get(0, 0, 0, Cube2), 0),
    ?assertEqual(cube:get(0, 0, 1, Cube2), 1),
    ?assertEqual(cube:get(0, 0, 2, Cube2), 2),
    ?assertEqual(cube:get(0, 1, 0, Cube2), 1),
    ?assertEqual(cube:get(0, 1, 1, Cube2), 2),
    ?assertEqual(cube:get(0, 1, 2, Cube2), 3).
    


			     
		
		    
		    
    
		     
