-module(mpl).
-behaviour(gen_server).


-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ----------
%% Interface
%% ----------

compute(Inputs) ->
    gen_server:call(?MODULE, {compute, Inputs}).

train(Training_values, Training_constant) ->
    gen_server:call(?MODULE, {train, Training_values, Training_constant}).

%% -------------------------------
%% implémentation du comportement
%% -------------------------------

init({Layers_values, Nb_inputs, Nb_layer}) -> 
    %% initialisation de l'état
    Network_value = neuron:make_network(Layers_values, Nb_inputs, Nb_layer),
    [{Nb_outputs, _, _} | _] = Layers_values,
    State = {Network_value, ready, [], {0, Nb_outputs}},

    %% Initialisation du processus de sortie
    Output = spawn(fun() -> neuron:output_serv(?SERVER) end),
    {_, _, Output_list, _} = Network_value,
    neuron:connect_output(Output, Output_list)
    {ok, State}.


handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.

%% -----------------------------------
%% Fonction de manipulation de l'état
%% -----------------------------------

adding_output(Output, State) ->
    {Network_value, Network_State, Output_list, {Outputs_computed, Nb_outputs}} = State,
    New_output_list = [Output | Output_list],
    {Network_value, Network_State, New_Output_list, {Outputs_computed + 1, Nb_outputs}}

is_output_ok(State) ->
    {_, _, _, {Outputs_computed, Nb_outputs}} = State,
    (Outputs_computed =:= Nb_outputs).

get_answer_info(State) ->
    {_, {_, From}, Output_list, _} = State,
    F = fun({A, _}, {B, _}) -> A =< B end,
    F2 = fun ({_, Val}) -> Val end,
    Ordoned_list = lists:map(F2, lists:sort(F, Output_list)),
    {From, Ordoned_list}.

get_network_state(State) ->
    {_, {Network_State, _}, _, _} = State,
    Network_State.

set_network_state(New_network_state, State) ->
    {Network_value, {Network_State, From}, Output_list, Output_info} = State,
    {Network_value, {New_network_State, From}, Output_list, Output_info}.
    
