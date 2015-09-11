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
    %% initialisation du réseau
    Network_value = neuron:make_network(Layers_values, Nb_inputs, Nb_layer),
    {Network, Input_list, Output_list, Network_size} = Network_value,

    %% Initialisation du trainer
    Trainer = trainer:init(Network, Network_size, Input_list),

    %% initialisation de l'état du serveur
    [{Nb_outputs, _, _} | _] = Layers_values,
    State = init_state(Network_value, Nb_outputs, Trainer)

    %% Initialisation du processus de sortie
    Output = spawn(fun() -> neuron:output_serv(?SERVER) end),
    neuron:connect_output(Output, Output_list)

    {ok, State}.


handle_call({compute, Values_list}, From, State) -> 
    case get_network_state(State) of
	%% si le réseau est disponible, on lance le calcul
	ready -> 
	    Input_list = get_input_list(State),
	    F = fun ({Input, Val}) -> Input ! {input, Val} end,
	    ok = lists:foreach(F, lists:zip(Inputs_list, Values_list)),
	    New_state = set_network_state(is_computing, State) ->
	    {reply, launched, New_state};
	%% sinon on renvoi une réponse négative
	Other -> {reply, Other, State}
    end.

handle_call({train, Training_values, Training_constant}, State) -> 
    case get_network_state(State) of
	%% si le réseau est disponible, on lance l'entrainement
	ready -> 
	    {Trainer, Input_list} = get_training_value(State),
	    trainer:launch(Trainer, Input_list, Training_values, Training_constant)
	%% sinon on renvoi une réponse négative
	Other -> {reply, Other, State}
    end.


handle_cast({output, {Rank, Value}}, State) -> 
    New_state = add_output(Output, State),
    if 
	is_output_ok(New_state) ->
	    {Client, Ordoned_list} = get_answer_info(State),
	    gen_server:reply(Client, {compute_done, Ordoned_list}),
	    {noreply, reset_state(New_state)};
	
	true ->
	    {noreply, New_state}
    end.
	    


handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.

%% -----------------------------------
%% Fonction de manipulation de l'état
%% -----------------------------------

init_state(Network_value, Nb_outputs, Trainer) ->
    State = {Network_value, Trainer, ready, [], {0, Nb_outputs}}.

reset_state(State) ->
    {Network_value, Trainer, _, _, {_, Nb_outputs}} = State,
    {Network_value, Trainer, ready, [], {0, Nb_outputs}}.

add_output(Output, State) ->
    {Network_value, Trainer, Network_State, Output_list, {Outputs_computed, Nb_outputs}} = State,
    New_output_list = [Output | Output_list],
    {Network_value, Trainer, Network_State, New_Output_list, {Outputs_computed + 1, Nb_outputs}}

get_input_list(State) ->
    {Network_value, _, _, _},
    {_, Input_list, _, _} = Network_value,
    Input_list.

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
    {Network_value, Trainer, {Network_State, From}, Output_list, Output_info} = State,
    {Network_value, Trainer, {New_network_State, From}, Output_list, Output_info}.

get_training_value(State) ->
    {Network_value, Trainer, _, _, _} = State,
    {_, Input_list, _, _} = Network_value,
    {Trainer, Input_list}.
    
    
    
