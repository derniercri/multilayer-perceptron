-module(matrix).
-compile(export_all).

%% create à new matrix of Lo*La dimension
new(Lo, La) ->
    Matrix = array:new(Lo),
    F = fun(_, _) -> array:new(La) end,
    array:map(F, Matrix).

new(Lo, La, Options) ->
    Matrix = array:new(Lo),
    F = fun(_, _) -> array:new(La, Options) end,
    array:map(F, Matrix).

%% create a new matrix with variable dimension for each line
new_variable(Lo, Size_list) ->
    F = fun(Size, {Array, I}) ->
		New_array = array:set(I, array:new(Size), Array),
		{New_array, I + 1}
	end,
    {Array, _} = lists:foldl(F, {array:new(Lo), 0}, Size_list),
    Array.


%% give the Matrix's size in a tuple {Lo, La}
size(Matrix) ->
    case array:size(Matrix) of
        0 -> {0, 0};
        Lo -> 
            La_array = array:get(0, Matrix),
            {Lo, array:size(La_array)}
    end.

verif_index(I, J, Matrix) ->
    case array:size(Matrix) of
	Max_i when (I >= 0) and (I < Max_i) ->
	    Line_i = array:get(I, Matrix),
	    Max_j = array:size(Line_i),
	    not (J >= 0) and (J < Max_j);
	_ -> true
    end.

%% set entry {I, J} to Value in the Matrix 
set(I, J, Value, Matrix) ->
    Verif_index = verif_index(I, J, Matrix),
    if 
        Verif_index ->  error(badarg);
        true ->
            La_array = array:get(I, Matrix),
            New_la_array = array:set(J, Value, La_array),
            array:set(I, New_la_array, Matrix)
    end.

%% get the value of entry {I, J}
get(I, J, Matrix) ->
    Verif_index = verif_index(I, J, Matrix),
    if 
        Verif_index ->  error(badarg);
        true ->
            array:get(J, array:get(I, Matrix))
    end.

%% F (I, J, Val, Acc) -> New Acc
foldl(F, Acc_init, Matrix) ->
    F_curry = fun(I) -> fun(J, Val, Acc) -> F(I, J, Val, Acc) end end,
    F_lo = fun(I, Val, Acc) -> array:foldl(F_curry(I), Acc, Val) end,
    array:foldl(F_lo, Acc_init, Matrix).

%% F (I, J, Val) -> New_val
map(F, Matrix) ->
    F_curry = fun(I) -> fun(J, Val) -> F(I, J, Val) end end,
    F_lo = fun(I, Val) -> array:map(F_curry(I), Val) end,
    array:map(F_lo, Matrix).
