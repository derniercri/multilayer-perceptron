-module(trainer).
-compile(export_all).

%%entraine un resaux (représenté par ses entrée et ses sortie).
%% Trainer_pid : pid du processus trainer
%% Inputs : liste des PID des entrée du résaux de neurone
%% Training_list : liste de couple entrée sortie à présenter au résaux de neurone
%% Training_constants : {Threshold, Primus_F, Speed}
%%                  Threshold : marge d'erreur acceptable par l'entraineur
%%                  Primus_F : dérivée de la fonction d'activation du neuronne
%%                  Speed : vitesse d'aprentissage

train(Trainer_pid, Inputs, Training_list, Training_constants) ->
    train(Trainer_pid, Inputs, Training_constants, Training_list, [], []).

train(_, _, _, [], [], _) ->
    ok;

train(Trainer_pid, Inputs, Training_constants, [], False_list, True_list) ->
    Training_list = lists:concat(False_list, True_list),
    train(Trainer_pid, Inputs, Training_constants, Training_list, [], []);

train(Trainer_pid, Inputs, Training_constants, [Training_values | Training_list], False_list, True_list ) ->
    {Training_inputs, Training_outputs} = Training_values,
    New_training_constants = add_training_output(Training_constants, Training_outputs),
    Trainer_pid ! {train, self(), New_training_constants},
    F = fun ({Input, Val}, _) ->
		Input ! {Val, input},
		ok
	end,
    lists:foldl(F, ok, lists:zip(Inputs, Training_inputs)),
    receive
	ok -> 
	    train(Trainer_pid, Inputs, Training_constants, Training_list, False_list, [Training_values | True_list]);
	updated ->
    	    train(Trainer_pid, Inputs, Training_constants, Training_list, [Training_values | False_list], True_list)
    end.

add_training_output (Training_constant, Training_output) ->
    {A, B, C} = Training_constant,
    {A, B, C, Training_output}.


%% AF
%%fonction pour le processus d'entrainement
%% Network_value : {Nb_neuron, Nb_layer, Weights}
%%      Nb_neuron : nombre de neurone dans le resaux
%%      Nb_layer : nombre de couche dans le resaux
%%      Weights : table de hachage contenant les poids. les clés sont un couple (layer, rak)
%% Quand il a reçut un jeu de test, attend les sortie des différent neurone puis rétropropage les erreur
%% puis il se remet en attente d'un jeu de test

trainer(Network_value) ->
    receive 
	{train, From, Training_constants} ->
	   trainer(From, Training_constants, gb_trees:empty(), Network_value, 0) 
    end.


trainer(Output, Training_constants, Outputs, Network_value, Outputs_received) ->
    case Network_value of
	{Outputs_received, _, _} ->
	    Output ! backpropagation(Training_constants, Outputs, Network_value),
	    trainer(Network_value);
	_ ->
	    receive
		{done, Result, {Neuron, Layer, Rank}} ->
		    New_outputs = gb_trees:insert({Layer, Rank}, {Neuron, Result}),
		    trainer(Output, Training_constants, New_outputs, Network_value, Outputs_received + 1)
	    end
    end.



%% compare les sortie de chaque neurone au jeu de test et met a jours les poids de chaque neurone.
%% renvoi updated si les poids ont été mis a jour,
%% ok sinon
%% Real_outputs est un gb_trees avec un couple (couche/rang) en clef et un couple (PID, Valeur) comme valeur
backpropagation(Training_constants, Outputs, Network_value) ->
    ok.    

