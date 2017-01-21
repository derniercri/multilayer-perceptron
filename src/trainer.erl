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
%% @doc Define a trainer for a neural network


-module(trainer).
-export([init/3, launch/4]).


%% --------------------------------------------
%% Type declaration
%% --------------------------------------------

%% @type training_value() = {Input :: [integer()], Output :: [integer()]}.
%%       Training values used by the supervisor <br/>
%%       Values :
%%       <ul><li>Input : Oredred list of input values</li>
%%       <li>Output : Expected output values</li></ul>
-type training_value() :: 
	{
	  Input :: [integer()], 
	  Output :: [integer()]
	}.


%% @type training_constant() = {Threshold :: integer(), Primus_F :: fun ((float()) -> float()), Speed :: float(), Max_iter :: integer()}.
%%       Supervisor parameters <br/>
%%       Values :
%%       <ul><li>Threshold : Error threshold</li>
%%       <li>Primus_F : Derivation on the learning function</li>
%%       <li>Speed : Learning speed</li>
%%       <li> Max_iter : Max training iteration</li></ul>
-type training_constant() :: 
	{
	  Threshold :: integer(), 
	  Primus_F :: fun ((float()) -> float()), 
	  Speed :: float(), 
	  Max_iter :: integer()
	}.

%% ----------------------------------------------------------


%% @doc launch the supervisor.<br/>
%%      Parameters:
%%      <ul><li>Trainer_pid : PID of the supervisor</li>
%%      <li>Input_list : PID's list of supervisor input</li>
%%      <li>Training_values : Training 's values list  @see training_value()</li>
%%      <li>Training_constant : Supervisors configuration @see training_constant()</li></ul>
-spec launch(
	Trainer_pid :: pid(), 
	Input_list :: [pid()], 
	Training_values :: [training_value()], 
	Training_constants :: training_constant()
       ) -> ok | max.

launch(Trainer_pid, Inputs_network, Training_values, Training_constants) ->
    launch(Trainer_pid, Inputs_network, Training_values, Training_constants, [], 0).    

launch(_, _, _, {_, _, _, I}, _, I) ->
    max;
launch(Trainer_pid, Inputs_network, [], Training_constants, List, I) ->
    launch(
      Trainer_pid, 
      Inputs_network,
      utils:shuffle(List), 
      Training_constants,
      [], 
      I
     );

launch(
  Trainer_pid, 
  Inputs_network, 
  [Training_values | Training_list], 
  Training_constants, 
  List, 
  I) ->
    
    {Training_inputs, Training_outputs} = Training_values,
    New_training_constants = add_training_output(Training_constants, Training_outputs),
    Trainer_pid ! {train, self(), New_training_constants},
    F = fun ({Input, Val}, _) ->
		Input ! {input, Val},
		ok
	end,
    lists:foldl(F, ok, lists:zip(Inputs_network, Training_inputs)),
    receive
	_ ->
	    launch(
	      Trainer_pid, 
	      Inputs_network, 
	      Training_list, 
	      Training_constants, 
	      [Training_values | List], 
	      I + 1
	     )
    end.
    
add_training_output (Training_constants, Training_output) ->
    {A, B, C, _} = Training_constants,
    {A, B, C, array:from_list(Training_output)}.



%% @doc Initialize the supervisor and connect it to the neural network <br/>
%%      Arguments : 
%%      <ul><li>Network : a Matrix composed by  {PID, nb_inputs} (a neuron)</li>
%%      <li>Network_size : List of layer's size</li>
%%      <li>Input_list : PID's list of the inputs</li></ul>
-spec init(
	Network :: matrix:matrix(), 
	Network_size :: [integer()], 
	Input_list :: [pid()]
       ) -> pid().

init(Network, Network_size, Input_list) ->
    Self = self(),
    Trainer = spawn(fun() -> train_init(Network, Network_size, Self, Input_list) end),
    receive ok -> Trainer end.

train_init(Network, Network_size, Main, Input_list) ->
    N_inputs = lists:sum(Network_size),
    %% initialisation des poids de chaque resaux
    F = fun (_, _, {PID, N}) ->
		Weights = gen_weight(N),
		PID ! {update, Weights},
		PID ! {connect_output, self()},
		{PID, array:from_list(Weights)}
	end,
    New_network = matrix:map(F, Network),
    F2 = fun(PID) -> PID ! {connect_output, self()} end,
    lists:foreach(F2, Input_list),
    Main ! ok,
    train(New_network, N_inputs, Network_size).
		
gen_weight(N) -> gen_weight([], N).
gen_weight(Acc, 0) -> Acc;
gen_weight(Acc, I) -> 
    New_Acc = [rand:uniform() - 0.5 | Acc],
    gen_weight(New_Acc, I - 1).


train(Network, N_inputs, Network_size) ->
    receive
	{train, From, Training_constant} ->
	    {Threshold, _, Speed, Training_outputs} = Training_constant,
	    %% recuperation des sortie de chaque neurone
	    Inputs_matrix = wait_inputs(N_inputs, Network_size),

	    Real_outputs = array:get(0, Inputs_matrix),
	    case compute_error(Training_outputs, Real_outputs) of
		Error when Error >= Threshold ->
		    New_network = backpropagation(
				    Speed, 
				    Training_outputs, 
				    Inputs_matrix, 
				    Network
				   ),
		    From ! updated,
		    train(New_network, N_inputs, Network_size);
		_ ->
		    From ! ok,
		    train(Network, N_inputs, Network_size)
	    end
    end.


backpropagation(Speed, Training_outputs, Inputs_matrix, Network) ->		
    Size = array:size(Network),
    F = fun (I, _, Matrix) when I =:= Size -> Matrix;
	    (I, Layer, Matrix) ->
		F2 = fun (0, _, Line) -> Line;
			 (J, Input, Line) ->
			     case I of
				 0 ->
				     Val = compute_gradient_output(
					     Input, array:get(J - 1, Training_outputs)
					    ),
				     array:set(J - 1, Val, Line);
				 _ ->
				     Previous_gradient = array:get(I - 1, Matrix),
				     Sum = sum(I, J, Previous_gradient, Network),
					 Val = compute_gradient_hiden(Input, Sum),
				     array:set(J - 1, Val, Line)
			     end
		     end,
		Size_line = array:size(Layer) - 1,
		Acc = array:new(Size_line),
		New_Layer = array:foldl(F2,Acc , Layer),
		array:set(I, New_Layer, Matrix)
	end,

    Gradients_matrix = array:foldl(F, array:new(Size), Inputs_matrix),
    F_map = fun(I, J, {PID, Weights}) ->
		    Gradient = matrix:get(I, J, Gradients_matrix),
		    F_map2 = fun (K, W) ->
				     Input = matrix:get(I+1, K, Inputs_matrix),
				     W + Gradient * Input * Speed
			     end,
		    New_weights = array:map(F_map2, Weights),
		    PID ! {update, array:to_list(New_weights)},
		    {PID, New_weights}
	    end,
    matrix:map(F_map, Network).


compute_error(Target_values, Value) ->
    L1 = array:to_list(Target_values),
    L2 = tl(array:to_list(Value)),
    F = fun ({A, B}, Acc) -> math:pow((A - B),2) + Acc end,
    lists:foldl(F, 0, lists:zip(L1, L2)).


wait_inputs(N, Network_size) ->
    F_map = fun(Size) -> Size + 1 end,
    New_size = lists:map(F_map, Network_size),
    wait_inputs(N, matrix:new_variable(length(New_size), New_size), 0).

wait_inputs(N, Matrix, N) -> 
    F = fun(_, Array) -> array:set(0, 1, Array) end,
    array:map(F, Matrix);
wait_inputs(N, Matrix, I) -> 
    receive
	{done, Result, {_, Layer, Rank}} ->
	    New_matrix = matrix:set(Layer, Rank, Result, Matrix),
	    wait_inputs(N, New_matrix, I + 1)
    end.

compute_gradient_output(Value, Target_value) ->
    Value * (1 - Value) * (Target_value - Value).

compute_gradient_hiden(Value, Sum) ->
    Value * (1 - Value) * Sum.


sum(Layer, Rank, Previous_gradients, Network) ->
    L = array:get(Layer - 1, Network),
    F = fun(_, {_, Weight_array}) -> array:get(Rank, Weight_array) end,
    Previous_weights = array:map(F, L),
    dot_product(Previous_gradients, Previous_weights).

dot_product(A1, A2) ->
    L1 = array:to_list(A1),
    L2 = array:to_list(A2),
    F = fun({A, B}, Acc) -> (A * B) + Acc end,
    lists:foldl(F, 0, lists:zip(L1, L2)).
