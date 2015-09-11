-module(mpl).
-behaviour(gen_server).


-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% -------------
%% Interface
%% -------------

compute(Inputs) ->
    gen_server:call(?MODULE, {compute, Inputs}).

train(Training_values, Training_constant) ->
    gen_server:call(?MODULE, {train, Training_values, Training_constant}).

%% --------------------------------
%% implémentation du comportement
%% --------------------------------

init({Layers_values, Nb_inputs, Nb_layer}) -> 
    State = neuron:make_network(Layers_values, Nb_inputs, Nb_layer),
    {ok, State}

handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.
