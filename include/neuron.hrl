%% @type layer_value() = { N :: integer(), P :: integer(), F :: fun ((float()) -> float())}.
%%       tuple contenant les paramètres d'une couche <br/>
%%       Valeurs :  
%%       <ul><li> N : nombre de neurone sur la couche </li>
%%       <li> P : nombre d'entrée de la couche </li>
%%       <li> F : fonction d'activation de la couche </li></ul>

-type layer_value() :: { N :: integer(), P :: integer(), F :: fun ((float()) -> float())}.

%% @type network_value() = {Network :: matrix:matrix(), Input_list :: [pid()], Output_list :: [pid()], Network_size :: [integer()]}.
%%      tuple contenant les valeurs permettant d'interagir avec le resaux <br/>
%%      Valeurs : 
%%      <ul><li>Network : une matrice contenant un couple {PID, nb_inputs} représentant un neurone</li>
%%      <li>Input_list : liste des PIDs des entrées du réseaux</li>
%%      <li>Output_list : liste des PIDs de la couche de sortie du réseaux</li>
%%      <li>Network_size : liste des tailles de chaque couches</li></ul>
-type network_value() :: {Network :: matrix:matrix(), Input_list :: [pid()], Output_list :: [pid()], Network_size :: [integer()]}.

%% @type neuron_value() = {Nb_inputs :: integer(), Weights :: [float()], B :: float(), F :: fun ((float()) -> float())}.
%%       tuple contenant les valeurs d'un neurone<br/>
%%       Valeurs : 
%%       <ul><li>Nb_inputs : nombre d'entrée connectées au neurone</li>
%%       <li>Weights : liste des poids à appliquer aux entrées pour le calcul. Cette liste doit être de taille Nb_inputs et les poids doivent être rangés dans l'ordre croissant de leur indice.</li>
%%       <li>B : le biais du neurone</li>
%%       <li>F : la fonction d'activation</li></ul>
-type neuron_value() :: {Nb_inputs :: integer(), Weights :: [float()], B :: float(), F :: fun ((float()) -> float())}.
