% Multilayer_perceptron_library
% Copyright (C) 2016  Dernier Cri
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program. If not, see <http://www.gnu.org/licenses/>.

%% @author A. d'Azémar
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

-export_type([size/0]).

-type size() :: {
	    non_neg_integer(),
	    non_neg_integer(), 
	    non_neg_integer()
	   }.


%% @doc create à new cube of Lo*La*Ha dimension
-spec new(
	Lo::non_neg_integer(), 
	La::non_neg_integer(), 
	Ha::non_neg_integer()
       ) -> array:array(any()).

new(Lo, La, Ha) ->
    Cube = array:new(Lo),
    F = fun(_, _) -> array:new(Ha) end,
    F2 = fun(_, _) -> 
		 A = array:new(La),
		 array:map(F, A)
	 end,
    array:map(F2, Cube).

%% @doc create à new cube of Lo*La*Ha dimension with an option list
-spec new(
	Lo::non_neg_integer(), 
	La::non_neg_integer(), 
	Ha::non_neg_integer(), 
	Options::list()
       ) -> array:array(any()).

new(Lo, La, Ha, Options) ->
    Cube = array:new(Lo),
    F = fun(_, _) -> array:new(Ha, Options) end,
    F2 = fun(_, _) -> 
		 A = array:new(La),
		 array:map(F, A)
	 end,
    array:map(F2, Cube).

%% @doc give the Cube's size in a tuple {Lo, La, Ha}
-spec size(Cube::array:array(any())) -> size().

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
-spec check_index(
	I::non_neg_integer(), 
	J::non_neg_integer(), 
	K::non_neg_integer(), 
	Size::size()
       ) -> boolean().

check_index(I, J, K, Size) ->
    {Max_i, Max_j, Max_k} = Size,
    Non_out_of_bound = (I < Max_i) and (J < Max_j) and (K < Max_k),
    Non_neg = (I >= 0) and (J >= 0) and (K >= 0),
    not (Non_out_of_bound and Non_neg).


%% @doc Set entry {I, J, K} to Value in the Cube
-spec set(
	I::non_neg_integer(), 
	J::non_neg_integer(), 
	K::non_neg_integer(), 
	Value::term(), 
	Cube::array:array(any())
       ) -> no_return().
 
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
-spec get(
	I::non_neg_integer(), 
	J::non_neg_integer(), 
	K::non_neg_integer(), 
	Cube::array:array(any())
       ) -> any().

get(I, J, K, Cube) ->
    Check = check_index(I, J, K, cube:size(Cube)),
    if 
	Check ->  error(badarg);
	true ->
	    array:get(K, array:get(J, array:get(I, Cube)))
    end.

%% @doc Convert Cube's value to list
-spec get_as_list(
	I::non_neg_integer(), 
	J::non_neg_integer(), 
	Cube::array:array(any())
       ) -> list(term()).

get_as_list(I, J, Cube) ->    
    array:to_list( array:get(J, array:get(I, Cube))).

%% @doc Foldl for a cube
-spec foldl(
	Function, 
	Acc_init :: A, 
	Array :: 
	  array:array(any())
       ) -> B when Function :: fun(
	  (Index :: non_neg_integer(), 
	   Value :: _, Acc :: A
	  ) -> B
							    ).
foldl(F, Acc_init, Cube) ->
    F_curry = 
	fun(I, J) -> 
		fun(K, Val, Acc) -> F(I, J, K, Val, Acc) 
		end 
	end,
    F_lo = fun(I, Val, Acc) ->
		   F_la = fun(J, Val2, Acc2) -> array:foldl(F_curry(I, J), Acc2, Val2) end,
		   array:foldl(F_la, Acc, Val)
	   end,
    array:foldl(F_lo, Acc_init, Cube).

%% @doc Map for a cube
-spec map(
	Function, 
	Array :: array:array(any())
       ) -> array:array(any()) when Function :: fun(
	  (Index :: non_neg_integer(), 
	   Value :: _
	  ) -> _).

map(F, Cube) ->
    F_curry = fun(I, J) -> fun(K, Val) -> F(I, J, K, Val) end end,
    F_lo = fun(I, Val) ->
		   F_la = fun(J, Val2) -> array:map(F_curry(I, J), Val2) end,
		   array:map(F_la, Val)
	   end,
    array:map(F_lo, Cube).

%% @doc Create a cube from a list
-spec from_list(
	List::list(), 
	Lo::non_neg_integer(), 
	La::non_neg_integer(), 
	Ha::non_neg_integer()
       ) -> array:array(any()).

from_list(List, Lo, La, Ha) ->
    from_list(List, cube:new(Lo,La,Ha), Lo, La, Ha, 0, 0, 0).

%% @doc Create a cube from a list
-spec from_list(
	List::list(), 
	Cube::array:array(any()), 
	Lo::non_neg_integer(), 
	La::non_neg_integer(), 
	Ha::non_neg_integer(), 
	I::non_neg_integer(), 
	J::non_neg_integer(), 
	K::non_neg_integer()
       ) -> array:array(any()).

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
-spec update_line_from_list(
	I::non_neg_integer(), 
	J::non_neg_integer(), 
	List::[any()], 
	Cube::array:array(any())
       ) -> no_return(). 

update_line_from_list(I, J, List, Cube) ->
    La_array = array:get(I, Cube),
    New_la_array = array:set(J, array:from_list(List, La_array), La_array),
    array:set(I, New_la_array, Cube).
