-module(matrix).
-include("matrix.hrl").
-export([new/2,
	 new/3,
	 new_variable/2,
	 size/1,
	 set/4,
	 get/3,
	 foldl/3,
	 map/2]).
	 

%% @doc create a new matrix of Lo*La dimension
-spec new(Lo::integer(), La::integer()) -> matrix().
new(Lo, La) ->
    Matrix = array:new(Lo),
    F = fun(_, _) -> array:new(La) end,
    array:map(F, Matrix).

%% @doc create a new matrix of Lo*La dimension according to the given option
%% @see array:new/1
-spec new(Lo::integer(), La::integer(), Options::array:array_opts()) -> matrix().
new(Lo, La, Options) ->
    Matrix = array:new(Lo),
    F = fun(_, _) -> array:new(La, Options) end,
    array:map(F, Matrix).

%% @doc create a new matrix with variable dimension for each line
%%      Lo is the number of line.
%%      Size_list contain the size of each line
-spec new_variable(Lo::integer(), Size_list::[integer()]) -> matrix().
new_variable(Lo, Size_list) ->
    F = fun(Size, {Array, I}) ->
		New_array = array:set(I, array:new(Size), Array),
		{New_array, I + 1}
	end,
    {Array, _} = lists:foldl(F, {array:new(Lo), 0}, Size_list),
    Array.


%% @doc give the Matrix's size in a tuple {Lo, La}
-spec size(matrix()) -> {integer(), integer()}.
size(Matrix) ->
    case array:size(Matrix) of
        0 -> {0, 0};
        Lo -> 
            La_array = array:get(0, Matrix),
            {Lo, array:size(La_array)}
    end.

%% intern function
%% test if the index I and J are include in Matrix 
verif_index(I, J, Matrix) ->
    case array:size(Matrix) of
	Max_i when (I >= 0) and (I < Max_i) ->
	    Line_i = array:get(I, Matrix),
	    Max_j = array:size(Line_i),
	    not (J >= 0) and (J < Max_j);
	_ -> true
    end.


%% @doc set entry {I, J} to Value in the Matrix 
-spec set(I::integer(), J::integer(), Value::I, Matrix::matrix()) -> matrix(I).
set(I, J, Value, Matrix) ->
    Verif_index = verif_index(I, J, Matrix),
    if 
        Verif_index ->  error(badarg);
        true ->
            La_array = array:get(I, Matrix),
            New_la_array = array:set(J, Value, La_array),
            array:set(I, New_la_array, Matrix)
    end.

%% @doc get the value of entry {I, J} from Matrix
-spec get(I::integer(), J::integer(), Matrix::matrix()) -> matrix().
get(I, J, Matrix) ->
    Verif_index = verif_index(I, J, Matrix),
    if 
        Verif_index ->  error(badarg);
        true ->
            array:get(J, array:get(I, Matrix))
    end.

%% @doc Fold the elements of the array using the given function and initial accumulator value. The elements are visited in order from the lowest index to the highest
-spec foldl(F::fun((I::integer(), J::integer(), Val::any(), Acc::Type2) -> New_acc::Type2), Acc_init::Type2, Matrix::matrix()) -> Type2.
foldl(F, Acc_init, Matrix) ->
    F_curry = fun(I) -> fun(J, Val, Acc) -> F(I, J, Val, Acc) end end,
    F_lo = fun(I, Val, Acc) -> array:foldl(F_curry(I), Acc, Val) end,
    array:foldl(F_lo, Acc_init, Matrix).

%% @doc map the given function onto each element of the matrix. The element are visited in order from the lowest line to the higtest.
-spec map(F::fun((I::integer(), J::integer(), Val::any()) -> New_val::any()), Matrix::matrix()) -> matrix().
map(F, Matrix) ->
    F_curry = fun(I) -> fun(J, Val) -> F(I, J, Val) end end,
    F_lo = fun(I, Val) -> array:map(F_curry(I), Val) end,
    array:map(F_lo, Matrix).
