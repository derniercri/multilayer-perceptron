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

%% @author A. d'Azémard
%% @copyright 2016 <Dernier Cri>
%% @version 2.0
%% @doc Some helpers for the neural network implementation

-module(utils).
-export(
   [
    gb_trees_to_sorted_list/1, 
    threshold/1, 
    sigmoid/1,
    shuffle/1
   ]
  ).

%% @doc Convert gb_tree into ordered list
-spec gb_trees_to_sorted_list(Gb_Trees::gb_trees:tree(_)) -> [any()].
gb_trees_to_sorted_list(Gb_Trees) ->
    List = gb_trees:to_list(Gb_Trees),
    F = fun({R1, _}, {R2, _}) ->  R1 =< R2 end,
    Sorted_list = lists:sort(F, List),
    F2 = fun({_, V}) -> V end,
    lists:map(F2, Sorted_list).

%% @doc Define the threshold for a neuron
-spec threshold(any()) -> fun((any) -> -1 | 1).
threshold(S) ->
    fun (X) when X > S -> 1;
        (_) -> -1
    end.

%% @doc Apply a sigmoid function ton a value
-spec sigmoid(number()) -> number().
sigmoid(X) -> 1 / (1 + math:exp(-X)).

%% @doc Randomize a list
-spec shuffle(list()) -> list().
shuffle(L) ->
    [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].
