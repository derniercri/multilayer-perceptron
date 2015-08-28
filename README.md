[![Build Status](https://magnum.travis-ci.com/derniercri/Multilayer_perceptron_library.svg?token=p2JVdRH4ZTM3RrdbkBvo)("Build Status")](https://magnum.travis-ci.com/derniercri/Multilayer_perceptron_library)

# Multilayer_perceptron_library
Fournis des fonction pour créer et entraîner un perceptron multi-couche concurrent


# installation et documentation

* générer la documentation : make doc

# Creer un resaux de neurone avec le module neuron

## Creer et initialiser un réseau

Voici comment créer un réseau de neurone en initialisant toute les valeurs de chaque neurone. Cela est inutile si le réseau doit être entraîné car les valeurs seront générées aléatoirement.

Nous allons utiliser la fonction `neuron:make_layer_hard/3` pour créer chaque couche indépendamment.  
Cette fonction prend en argument :

- le rang de la couche (la couche d'entrée devant avoir l'indice le plus haut et la couche de sortie l'indice 0)

- La liste des PIDs des sorties à relier au neurone

- la liste des valeurs de chaque neurone (tuple décris plus bas). Les valeurs des neurones doivent y être rangées dans l'ordre croissant. Ainsi, la valeur du neurone 1 doit être entête de la liste.

### Valeurs de neurone

Les neurones sont représentés par un tuple contenant les valeurs nécessaires au calcul de son résultat. Il possède 4 champ :

- Nb_inputs : le nombre d'entrée connectées au neurone. C'est le nombre de neurone de la couche précédente ou le nombre d'entrée du réseau si le neurone fait partie de la couche d'entrée.

- Weights : la liste des poids appliqués à chaque entrée du neurone (il doit y en avoir autant que d'entrée). Ces poids doivent être triés dans le même ordre que les entrées. Ainsi le poids appliqué à l'entrée 1 doit être en première position.

- B : le biais du neurone

- F : fonction d'activation du neurone

### Exemple d'utilisation

Initialisation d'un réseau réalisant un xor.  
La fonction d'activation est la fonction seuil du module utils  
Le processus courant est utilisé comme sortie pour le réseau.  
![Example](https://github.com/derniercri/Multilayer_perceptron_library/img/xor.png "Xor example")
```erlang
%% Création des neurones
N1 = {2, [2, 2], -1, utils:seuil(0)},
N2 = {2, [-1, -1], 1.5, utils:seuil(0)},
N3 = {2, [1, 1], -1.5, utils:seuil(0)},  
%% Création des couches
C1 = neuron:make_layer_hard(0, [self()],[N3),
C2 = neuron:make_layer_hard(1, C1, [N1, N2]).
```

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

