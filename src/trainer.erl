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
    Trainer_pid ! {train, self(), Training_outputs, Training_constants},
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


%% AF
%%fonction pour le processus d'entrainement
%% Se met en attente d'un jeu de test
%% Quand il a reçut un jeu de test, attend les sortie des différent neurone puis rétropropage les erreur
%% puis il se remet en attente d'un jeu de test

trainer(Nb_outputs) ->
    receive 
	{train, From, Training_outputs, Training_constants} ->
	   trainer(From, Training_outputs, gb_trees:empty(), Nb_outputs, 0) 
    end.


trainer(Output, Training_outputs, Real_outputs, Nb_outputs, Nb_outputs) ->
    Output ! backpropagation(Training_outputs, Real_outputs),
    trainer(Nb_outputs);

trainer(Output, Training_outputs, Real_outputs, Nb_outputs, Outputs_received) ->
    receive
	{done, Result, {Neuron, Layer, Rank}} ->
	    New_real_outputs = gb_trees:insert({Layer, Rank}, {Neuron, Result}),
	    trainer(Output, Training_outputs, New_real_outputs, Nb_outputs, Outputs_received)
    end.

	
%% compare les sortie de chaque neurone au jeu de test et met a jours les poids de chaque neurone.
%% renvoi updated si les poids ont été mis a jour,
%% ok sinon
%% Real_outputs est un gb_trees avec un couple (couche/rang) en clef et un couple (PID, Valeur) comme valeur
backpropagation(Training_outputs, Real_outputs) ->
    ok.    

