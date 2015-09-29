-module(mpl).
-behaviour(gen_server).


-export([start_link/3, compute/1, train/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start_link(Layers_values, Nb_inputs, Nb_layer) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Layers_values, Nb_inputs, Nb_layer}, []).

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
    Trainer = spawn(fun() -> trainer_init(Network, Network_size, Input_list, ?MODULE) end),

    %% initialisation de l'état du serveur
    [{Nb_outputs, _, _} | _] = Layers_values,
    State = init_state(Network_value, Nb_outputs, Trainer, Output_list),
    {ok, State}.


handle_call({compute, Values_list}, From, State) -> 
    case get_network_state(State) of
	%% si le réseau est disponible, on lance le calcul
	ready -> 
	    Inputs_list = get_input_list(State),
	    F = fun ({Input, Val}) -> Input ! {input, Val} end,
	    ok = lists:foreach(F, lists:zip(Inputs_list, Values_list)),
	    New_state = set_network_state(is_computing, State),
%%	    io:format("compute launched~n"),
	    {reply, launched, add_from(New_state, From)};
	%% sinon on renvoi une réponse négative
	Other -> {reply, Other, State}
    end;
handle_call({train, Training_values, Training_constant}, From, State) -> 
    case get_network_state(State) of
	%% si le réseau est disponible, on lance l'entrainement
	init -> 
	    {Trainer, Input_list} = get_training_value(State),
	    Trainer ! {train, Input_list, Training_values, Training_constant},
	    New_state = set_network_state(is_training, State),
	    %% io:format("training launched~n"),
	    {reply, launched, add_from(New_state, From)};
	%% sinon on renvoi une réponse négative
	Other -> {reply, Other, State}
    end.


handle_cast({output, Output}, State) -> 
    case get_network_state(State) of
	is_training -> {noreply, State};
	_ ->
	    New_state = add_output(Output, State),
	    case is_output_ok(New_state) of
		true ->
		    {Client, Ordoned_list} = get_answer_info(New_state),
		    %% io:format("Output ~p~n", [Ordoned_list]),
		    gen_server:reply(Client, {compute_done, Ordoned_list}),
		    {noreply, reset_state(New_state)};

		_ ->
		    {noreply, New_state}
	    end
    end;
handle_cast({training_done, Result}, State) -> 
    Client = get_from(State),
    gen_server:reply(Client, {training_done, Result}),
    %% Initialisation du processus de sortie
    Output_pid_list = get_output_pid_list(State),
    Output = spawn(fun() -> neuron:output_serv(?MODULE) end),
    neuron:connect_output(Output, Output_pid_list),

    %% Réinitialisation de l'état
    New_state = reset_state(State),
    %% io:format("training end~n"),
    {noreply, New_state}.


handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.


code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% -----------------------------------
%% Fonction de manipulation de l'état
%% -----------------------------------

init_state(Network_value, Nb_outputs, Trainer, Output_pid_list) ->
    {Network_value, Trainer, {init, null}, [], {0, Nb_outputs}, Output_pid_list}.

reset_state(State) ->
    {Network_value, Trainer, _, _, {_, Nb_outputs}, Output_pid_list} = State,
    {Network_value, Trainer, {ready, null}, [], {0, Nb_outputs},Output_pid_list}.

add_output(Output, State) ->
    {Network_value, Trainer, Network_State, Output_list, {Outputs_computed, Nb_outputs}, Output_pid_list} = State,
    New_output_list = [Output | Output_list],
    {Network_value, Trainer, Network_State, New_output_list, {Outputs_computed + 1, Nb_outputs}, Output_pid_list}.

get_input_list(State) ->
    {Network_value, _, _, _, _, _} = State,
    {_, Input_list, _, _} = Network_value,
    Input_list.

is_output_ok(State) ->
    {_, _, _, _, {Outputs_computed, Nb_outputs}, _} = State,
    (Outputs_computed =:= Nb_outputs).

get_answer_info(State) ->
    {_, _, {_, From}, Output_list, _, _} = State,
    F = fun({A, _}, {B, _}) -> A =< B end,
    F2 = fun ({_, Val}) -> Val end,
    Ordoned_list = lists:map(F2, lists:sort(F, Output_list)),
    {From, Ordoned_list}.

get_from(State) ->
    {_, _, {_, From}, _, _, _} = State,
    From.    

get_network_state(State) ->
    {_, _, {Network_State, _}, _, _, _} = State,
    Network_State.

set_network_state(New_network_state, State) ->
    {Network_value, Trainer, {_, From}, Output_list, Output_info, Output_pid_list} = State,
    {Network_value, Trainer, {New_network_state, From}, Output_list, Output_info, Output_pid_list}.

get_training_value(State) ->
    {Network_value, Trainer, _, _, _, _} = State,
    {_, Input_list, _, _} = Network_value,
    {Trainer, Input_list}.
    
add_from(State, From) ->
    {Network_value, Trainer, {Network_state,_}, Output_list, Output_info, Output_pid_list} = State,
    {Network_value, Trainer, {Network_state, From}, Output_list, Output_info, Output_pid_list}.
    
get_output_pid_list(State) ->
    {_, _, _, _, _, Output_pid_list} = State,
    Output_pid_list.

%% -------------------------------------
%% Fonction d'interface avec le trainer
%% -------------------------------------

trainer_init(Network, Network_size, Input_list, Serv_name) ->
    Trainer = trainer:init(Network, Network_size, Input_list),
    train_proc(Trainer, Serv_name).

train_proc(Trainer, Serv_name) ->
    receive 
	{train, Input_list, Training_values, Training_constant} ->
	    Result = trainer:launch(Trainer, Input_list, Training_values, Training_constant),
	    gen_server:cast(Serv_name, {training_done, Result});
	_ -> error(wrong_message)
    end.
