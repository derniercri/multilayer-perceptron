> This readme is work in progress

# Multilayer_perceptron_library

> This library is a small **experimentation** about neural network and actor model. 
> The module is not used in production.  
> The library provides functions for create and train multi-layer perceptron.

## Installation 

This project use rebar3, so : 

-  `rebar3 compile` compile the project ; 
-  `rebar3 eunit` run the tests ;
-  `rebar3 edoc` generate the documentation in `doc/` ;
-  `rebar3 dialyzer` type check the library ; 
-  `rebar3 shell` run an Erlang shell with the library loaded.

## Neuron module

[Link to the documentation](http://derniercri.github.io/multilayer_perceptron_library/index.html)

Here is how to create a neural network by initializing all the values of each neuron. This is useless if the network is to be trained because the values will be generated randomly.

The function `neuron:make_layer_hard/3` create a layer.

-  `Rank` : the ID of a layer (`0` is `output`, `> 0` are `inputs`) ; 
-  `Pid list` : the list of the PID's to be relied to the neuron ;
-  `Neurons list`: a list of ordered neurons.

### Neuron representation

A neuron is a tuple with 4 fields : 

-  `Nb_inputs` : The number of inputs connected to the neuron. This is the neuron number of the previous layer or the number of network input if the neuron is part of the input layer ;
-  `Weights ` : the list of weights applied to each input of the neuron (there must be as many as input). These weights must be sorted in the same order as the entries. Thus the weight applied to the inlet 1 must be in the first position ; 
-  `B` : The threshold of the neuron ; 
-  `F` : Activation function of the neuron.

### Example
Initialization of a network performing a `xor`.
The activation function is the threshold function of the 
[utils module](http://derniercri.github.io/multilayer_perceptron_library/utils.html#threshold-1). 
The current process is used as the output for the network.

![Example](https://github.com/derniercri/multilayer_perceptron_library/blob/master/img/schema2.png?raw=true)
```erlang
%% Neuron's creation
N1 = {2, [2, 2], -1, utils:threshold(0)},
N2 = {2, [-1, -1], 1.5, utils:threshold(0)},
N3 = {2, [1, 1], -1.5, utils:threshold(0)},  
%% Layer's creation
C1 = neuron:make_layer_hard(0, [self()],[N3),
C2 = neuron:make_layer_hard(1, C1, [N1, N2]).
```

## Create an empty network to be trained

In the case of a training, a network could be initialized with empty values, using 
[`neuron:make_network/3`](http://derniercri.github.io/multilayer_perceptron_library/neuron.html#make_network-3) : 

-  `Layer_values` : list of parameters for each layer (see [layer_value](http://derniercri.github.io/multilayer_perceptron_library/neuron.html#types)) for a more detailed description). The parameters must be arranged in ascending order (the output layer with the index 0 must therefore be placed at the top of the list) ;
-  `Nb_inputs` : the number of inputs of the network ;
-  `Nb_layer ` : the number of layers of the network.

-----------


## Créer un réseau vide en vue d'un entraînement

Dans le cas d'un entraînement, le réseau peut être initialiser avec des valeurs vide grâce à la fonction `neuron:make_network/3`  
Cette fonction demande trois arguments :

- Layers_values : liste des paramètres de chaque couche (voir le type neuron:layer_value() pour une description plus détaillée). Les paramètres doivent être rangés dans l'ordre croissant, (la couche de sortie ayant l'indice 0 doit donc être placée en tête de la liste)

- Nb_inputs : Le nombre d'entrée du réseau.

- Nb_layer : le nombre de couche sur le réseau.

### Exemple d'utilisation

Nous allons créer la même structure que précédemment mais sans ses valeurs

```erlang
F = fun (X) -> utils:sigmoid(X) end,
L = [{1, 2, F},
{2, 2, F}],
{Network, Input_list, Output_list, Network_size} = neuron:make_network(L, 2, 2).
```

Les valeurs renvoyées par `make_network` nous seront utiles pour communiquer avec le réseau et l'entraîner.

La valeurs Output_list contient les PIDs des neurones de la couche de sortie. cette liste doit être passée en argument de la fonction `neuron:connect_output/2` pour être connecté à une sortie.  
Attention, ne connectez vos sorties qu'une fois que le réseau à terminé son entraînement, sinon votre processus de sortie sera submergé de message.

## Interagir avec le resaux

### utiliser les entrées

Pour envoyer des informations au réseau, il faut utiliser un processus externe ,relié à la couche d'entrée du réseau et lui envoyer les information à transmettre. l'information seras alors transmises à tout les neurones de la couche d'entrée.

Si le réseau à été créé grâce à la fonction `make_network`, la liste des PIDs des processus utilisés comme entrée est renvoyé dans la valeur `Input_list` (voir plus haut)

Si le réseau à été initialisé manuellement, il faut créer manuellement les entrées.  
Il suffit de créer un processus et de lui faire utiliser la fonction `neuron:input/3`.

#### Exemple
Nous créons ici 2 entrée que nous raccordons au réseau créé manuellement

```erlang
Input1 = spawn(fun () -> neuron:input(2, 1, C2) end),
Input2 = spawn(fun () -> neuron:input(2, 2, C2) end).
```
#### Envoyer une valeur à une entrée
Pour envoyer une valeur à une entrée il suffit d'envoyer le message `{input, Valeur}` au processus créé. Par exemple pour envoyer la valeur 1 à l'entrée Input1 créée précédemment : `Input1 ! {input, 1}`

### récupérer les valeurs de sortie du réseau.
Une fois ces calculs terminés, le réseau enverras ces résultats aux processus spécifiés. (ces processus sont spécifié à la création si vous avez créé le neurone manuellement ou grâce à la fonction `neuron:connect_output/2` si vous avez initialisé un réseau vide). Le message envoyé est : `{done, Result, {PID, Layer, Rank}}` avec :

- Result : le résultat du calcul

- PID : le PID du neurone ayant envoyé le résultat

- Layer : le rang de la couche ayant envoyée le résultat

- Rank :  le rang du neurone ayant envoyé le résultat

# Entraîner un réseau

Pour cette étape nous partirons du principe que nous possédons le réseau initialisé précédemment.

## Créer et initialiser le superviseur

Pour créer le superviseur on va réutiliser les valeurs renvoyées par la fonction `neuron:make_network` :

```erlang
Trainer = trainer:init(Network, Network_size, Input_list).
```

Cette fonction renvoie le PID du superviseur.

## Lancer l'entraînement

Pour lancer l'entraînement, il faut tout d'abord initialiser plusieurs valeurs. Les valeurs d'entraînements et les constantes d'entraînement. (voir la documentation des type `training_value` et `training_constant` du module `trainer`)

Dans notre exemple, nous voulons recréer un xor. Nous allons initialiser notre superviseur avec une marge d'erreur de 0, une vitesse de 1 et un maximum d'itération de 10000.

Pour lancer l'entraînement, nous utiliserons la fonction `trainer:launch/4`

```erlang
%% constante d'entraînement
Threshold = 0,
Primus_F = F,
Speed = 1,
Max_iter = 10000,
%% Valeur d'entraînement
Training_list = [ {[1,0], [1]}, {[0,1], [1]}, {[0,0], [0]}, {[1,1], [0]}],
%% Lancement de l'entraînement
trainer:launch(Trainer, Input_list, Training_list, {Threshold, Primus_F, Speed, Max_iter}).
```

Maintenant que notre réseau est entraîné, nous pouvons utiliser la fonction connect_output pour recueillir les résultat avec le processus principal :

```erlang
neuron:connect_output(self(), Output_list)
```
