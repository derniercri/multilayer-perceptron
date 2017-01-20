%% @author A. d'Azémard
%% @copyright 2016 <Dernier Cri>
%% @version 2.0
%% @doc Define a cube type (an alias of scoped array)

-module(cube).
-export(
   [
    new/3,
    new/4, 
    size/1, 
    check_index/4,
    set/5,
    get/4, 
    get_as_list/3, 
    foldl/3,
    map/2, 
    from_list/4,
    from_list/8, 
    update_line_from_list/4
   ]
  ).

-export_type([array/0, size/0]).

-opaque array() :: atom().
-type size() :: {number(), number(), number()}.


%% @doc create à new cube of Lo*La*Ha dimension
-spec new(Lo::number(), La::number(), Ha::number()) -> array().
new(Lo, La, Ha) ->
    Cube = array:new(Lo),
    F = fun(_, _) -> array:new(Ha) end,
    F2 = fun(_, _) -> 
		 A = array:new(La),
		 array:map(F, A)
	 end,
    array:map(F2, Cube).

%% @doc create à new cube of Lo*La*Ha dimension with an option list
-spec new(Lo::number(), La::number(), Ha::number(), Options::list()) -> array().
new(Lo, La, Ha, Options) ->
    Cube = array:new(Lo),
    F = fun(_, _) -> array:new(Ha, Options) end,
    F2 = fun(_, _) -> 
		 A = array:new(La),
		 array:map(F, A)
	 end,
    array:map(F2, Cube).

%% @doc give the Cube's size in a tuple {Lo, La, Ha}
-spec size(Cube::array()) -> size().
size(Cube) ->
    case array:size(Cube) of
	0 -> {0, 0, 0};
	Lo -> 
	    La_array = array:get(0, Cube),
	    case array:size(La_array) of
          0 -> {Lo, 0, 0};
          La ->
              Ha_array = array:get(0, La_array),
              case array:size(Ha_array) of
                  0 -> {Lo, La, 0};
                  Ha -> {Lo, La, Ha}
              end
		  end
	  end.

%% @doc Check if an index is valid
-spec check_index(I::number(), J::number(), K::number(), Size::size()) -> boolean().
check_index(I, J, K, Size) ->
    {Max_i, Max_j, Max_k} = Size,
    Non_out_of_bound = (I < Max_i) and (J < Max_j) and (K < Max_k),
    Non_neg = (I >= 0) and (J >= 0) and (K >= 0),
    not (Non_out_of_bound and Non_neg).


%% @doc Set entry {I, J, K} to Value in the Cube
-spec set(I::number(), J::number(), K::number(), Value::term(), Cube::array()) -> no_return(). 
set(I, J, K, Value, Cube) ->
    Check = check_index(I, J, K, cube:size(Cube)) ,
    if 
	Check->  error(badarg);
	true ->
	    La_array = array:get(I, Cube),
	    Ha_array = array:get(J, La_array),
	    New_ha_array = array:set(K, Value, Ha_array),
	    New_la_array = array:set(J, New_ha_array, La_array),
	    array:set(I, New_la_array, Cube)
    end.

%% @doc Get the value of entry {I, J, K}
-spec get(I::number(), J::number(), K::number(), Cube::array()) -> any().
get(I, J, K, Cube) ->
    Check = check_index(I, J, K, cube:size(Cube)),
    if 
	Check ->  error(badarg);
	true ->
	    array:get(K, array:get(J, array:get(I, Cube)))
    end.

%% @doc Convert Cube's value to list
-spec get_as_list(I::number(), J::number(), Cube::array()) -> list(term()).
get_as_list(I, J, Cube) ->    
    array:to_list( array:get(J, array:get(I, Cube))).

%% @doc Foldl for a cube
-spec foldl(Function, Acc_init :: A, Array :: array()) -> B when
      Function :: fun((Index :: number(), Value :: _, Acc :: A) -> B).
foldl(F, Acc_init, Cube) ->
    F_curry = fun(I, J) -> fun(K, Val, Acc) -> F(I, J, K, Val, Acc) end end,
    
    F_lo = fun(I, Val, Acc) ->
		   F_la = fun(J, Val2, Acc2) -> array:foldl(F_curry(I, J), Acc2, Val2) end,
		   array:foldl(F_la, Acc, Val)
	   end,
    array:foldl(F_lo, Acc_init, Cube).

%% @doc Map for a cube
-spec map(Function, Array :: array()) -> array() when
Function :: fun((Index :: number(), Value :: _) -> _).
map(F, Cube) ->
    F_curry = fun(I, J) -> fun(K, Val) -> F(I, J, K, Val) end end,
    F_lo = fun(I, Val) ->
		   F_la = fun(J, Val2) -> array:map(F_curry(I, J), Val2) end,
		   array:map(F_la, Val)
	   end,
    array:map(F_lo, Cube).

%% @doc Create a cube from a list
-spec from_list(List::list(), Lo::number(), La::number(), Ha::number()) -> array().
from_list(List, Lo, La, Ha) ->
    from_list(List, cube:new(Lo,La,Ha), Lo, La, Ha, 0, 0, 0).

%% @doc Create a cube from a list
-spec from_list(
	List::list(), 
	Cube::array(), 
	Lo::number(), 
	La::number(), 
	Ha::number(), 
	I::number(), 
	J::number(), 
	K::number()) -> array().
from_list([], Cube, _, _, _, _, _, _) -> Cube;
from_list(List, Cube, Lo, La, Ha, I, J, K) when K >= Ha ->
    if
	J =:= La - 1-> 
	    if I =:= Lo - 1 -> error(index_out_of_bound);
	       true -> from_list(List, Cube, Lo, La, Ha, I + 1, 0, 0)
	    end;
	true -> from_list(List, Cube, Lo, La, Ha, I, J+1, 0)
    end;
from_list([Val| Tail], Cube, Lo, La, Ha, I, J, K) -> 
    io:format("~p~n", [{I, J, K}]),
    New_cube = cube:set(I, J, K, Val, Cube),
    from_list(Tail, New_cube, Lo, La, Ha, I, J, K+1).

  
%% @doc Update a line from a list
-spec update_line_from_list(I::number(), J::number, List::list(), Cube::array()) -> no_return(). 
update_line_from_list(I, J, List, Cube) ->
    La_array = array:get(I, Cube),
    New_la_array = array:set(J, array:from_list(List, La_array), La_array),
    array:set(I, New_la_array, Cube).
