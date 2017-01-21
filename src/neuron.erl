%% @author A. d'Azémar
%% @copyright 2016 <Dernier Cri>
%% @version 2.0
%% @doc Entry point to manage neural networks

-module(neuron).
-export(
   [
    make_network/3,
    make_layer_hard/3,
    output_progress/1,
    output/0,
    connect_output/2,
    input/3
   ]
  ).

%% --------------------------------------------
%% Type declaration
%% --------------------------------------------

%% @type layer_value() = { N :: integer(), P :: integer(), F :: fun ((float()) -> float())}.
%%       tuple with layer's parameters <br/>
%%       Values :  
%%       <ul><li> N : number of neurons on a layer</li>
%%       <li> P : number input's layer</li>
%%       <li> F : activation function of a layer</li></ul>

-type layer_value() :: 
	{ 
	  N :: integer(), 
	  P :: integer(), 
	  F :: fun ((float()) -> float())
	}.

-type network_value() :: 
	{
	  Network :: matrix:matrix(), 
	  Input_list :: [pid()], 
	  Output_list :: [pid()], 
	  Network_size :: [integer()]
	}.

-type neuron_value() :: 
	{
	  Nb_inputs :: integer(), 
	  Weights :: [float()], 
	  B :: float(), 
	  F :: fun ((float()) -> float())
	}.

%% --------------------------------------------

-spec output_progress(I :: integer()) -> no_return().
output_progress(I) -> output(0, I).

-spec output(N :: integer(), I :: integer()) -> no_return().
output(N, I) ->
    receive
	{done, _, _} -> 
	    if
		(N rem I) =:= 0->
		    io:format("~p steps~n",[N]),
		    output(N + 1, I);
		true ->
		    output(N + 1, I)
	    end;
	_ ->  
	    output(N + 1, I)
    end.

%% @doc Output a neuron
-spec output() -> no_return().
output() ->
    receive
        {done, Result, {_, Layer, Rank}} -> 
            io:format(
              "Layer : ~p, neuron ~p, result : ~p~n", 
              [Layer, Rank, Result]
             ),
            output();
        _ -> output()
    end.


%% @doc Connect an output to a neurons list
-spec connect_output(Output :: pid(), Layer :: [pid()]) -> ok.
connect_output(Output, Layer) ->
    F = fun(PID) -> PID ! {connect_output, Output} end,
    lists:foreach(F, Layer).


%% @doc Give to a processus a generic entry with the capacity to send values to one or more 
%% neurons.<br/>
-spec input(Layer :: integer(), Rank :: integer(), Outputs :: [pid()]) -> no_return().
input(Layer, Rank, Outputs) ->
    receive
        {input, Value} ->
            send_msg_to_list(Outputs, {done, Value, {self(), Layer, Rank}}),
            input(Layer, Rank, Outputs);
	{connect_output, From} -> 
	    input(Layer, Rank, [From | Outputs]);
        _ ->
	    input(Layer, Rank, Outputs)
    end.


%% @doc Create a network<br/>

-spec make_network(
	Layer_values :: [layer_value()], 
	Nb_inputs :: integer(), 
	Nb_layer :: integer()
       ) -> network_value().

make_network(Layer_values, Nb_inputs, Nb_layer) ->
    F = fun({Nb_neuron, Nb_previous, Fun}, {Network, I, Old_layer}) ->
		Layer = make_layer(I, Old_layer, Nb_neuron, Nb_previous, Fun),
		F2 = fun(PID, {J, Layer_array}) ->
			     New_array = array:set(J, {PID, Nb_previous + 1}, Layer_array),
			     {J + 1, New_array} 
		     end,
		{_, Layer_array} = lists:foldl(F2, {0, array:new(Nb_neuron)}, Layer),
		{array:set(I, Layer_array, Network), I + 1, Layer}
	end,
    {Network, _, Last_layer} = lists:foldl(F, {array:new(Nb_layer), 0, []}, Layer_values),
    Input_list = make_inputs(Nb_inputs, Nb_layer, Last_layer),
    Output_array = array:get(0, Network),
    F2 = fun(_, {PID, _}, List) -> [PID | List] end,
    Output_list = lists:reverse(array:foldl(F2, [], Output_array)),
    {Size, _, _} = lists:unzip3(Layer_values),
    Network_size = Size ++ [Nb_inputs],
    {Network, Input_list, Output_list, Network_size}.


make_inputs(N, Layer_rank, PID_list) ->  make_inputs(N, Layer_rank, PID_list, []).
make_inputs(0, _, _, Acc) -> Acc;
make_inputs(N, Layer_rank, PID_list, Acc) -> 
    Input = spawn( fun() -> neuron:input(Layer_rank, N, PID_list) end),
    make_inputs(N - 1, Layer_rank, PID_list, [Input | Acc]).


make_layer(Layer_rank, Outputs, Nb_neuron, Nb_inputs, Fun) ->
    Value = {Nb_inputs, [], 0, Fun},
    init_layer(Layer_rank, Outputs, Nb_neuron, Value).
    
init_layer(Layer_rank, Outputs, N, Value) -> init_layer(Layer_rank, Outputs, N, Value, []).
init_layer(_, _, 0, _, Layer) -> Layer;
init_layer(Layer_rank, Outputs, N, Value, Layer) ->
    Neuron = init_neuron(Outputs, Layer_rank, N, Value),
    init_layer(Layer_rank, Outputs, N - 1, Value, [Neuron | Layer]).



%% @doc Create a layer with the weight and the threshold for each neurons and returns 
%% a PID list
-spec make_layer_hard(
	Layer_rank :: integer(), 
	Outputs :: [pid()], 
	Neuron_values :: [neuron_value()]
       ) -> [pid()].

make_layer_hard(Layer_Rank, Outputs, Neuron_values) ->
    F = fun (Neuron_value, {Rank, Acc}) ->
                Neuron = init_neuron(Outputs, Layer_Rank, Rank, Neuron_value),
                {Rank + 1, [Neuron | Acc]}
        end,
    {_, L} = lists:foldl(F, {1, []}, Neuron_values),
    lists:reverse(L).


init_neuron(Outputs, Layer, Rank, Values) ->
    {N, W, B, F} = Values,
    New_values = {N, [B | W], F},
    %% insertion de l'entrée du biais
    Inputs = gb_trees:insert(0, 1, gb_trees:empty()),
    Env = {Outputs, Inputs, Layer, Rank, New_values},
    spawn(fun() -> run_neuron(Env, 0) end).


run_neuron(
  {
    Outputs, 
    Inputs, 
    Layer, 
    Rank, 
    {Nb_inputs, Weights, F}
  }, Nb_inputs
 ) ->
    Inputs_list = utils:gb_trees_to_sorted_list(Inputs),
    Result = compute({Weights, F}, Inputs_list),
    Msg = {done, Result, {self(), Layer, Rank}},
    ok = send_msg_to_list(Outputs, Msg),
    New_inputs = gb_trees:insert(0, 1, gb_trees:empty()),
    run_neuron({Outputs, New_inputs, Layer, Rank, {Nb_inputs, Weights, F}}, 0);

run_neuron(Env, Nb_inputs) ->
    {Outputs, Inputs, Layer, Rank, Values} = Env,    
    receive
        {done, Result, {_, _, Rank_N}} -> 
            run_neuron(
              {
                Outputs, 
                gb_trees:insert(Rank_N, Result, Inputs), 
                Layer, 
                Rank, 
                Values
              }, Nb_inputs + 1
	     );
        {update, New_weights} ->
            New_values = update_weights(Values, New_weights),
            run_neuron({Outputs, Inputs, Layer, Rank, New_values}, Nb_inputs);
        {get_weight, From} ->
            Weights = get_weights(Values),
            From ! {give_weight, Weights},
	    run_neuron(Env, Nb_inputs);
	{connect_output, PID} ->
	    New_env = {[PID | Outputs], Inputs, Layer, Rank, Values},
	    run_neuron(New_env, Nb_inputs)
    end.

get_weights({_, Weights, _}) -> Weights.

update_weights(Values, New_weights) ->
    {Nb_inputs, _, F} = Values,
    {Nb_inputs, New_weights, F}.

compute({W,F}, Input) ->
    io:format("~p  ~p~n", [Input, W]),
    Fun = fun ({Val, Weight}, Acc) -> Val * Weight + Acc end,
    L = lists:zip(Input, W),
    Val = lists:foldl(Fun, 0, L),
    F(Val).

send_msg_to_list(Pids, Msg) ->
    lists:foreach(fun(Pid) -> Pid ! Msg end, Pids).
