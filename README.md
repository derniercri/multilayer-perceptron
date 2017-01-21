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

### A xor without values 

```erlang
F = fun (X) -> utils:sigmoid(X) end,
L = [{1, 2, F},
{2, 2, F}],
{Network, Input_list, Output_list, Network_size} = neuron:make_network(L, 2, 2).
```

The values returned by `make_network` will help us communicate with the network and train it.

The `Output_list` values contain the PIDs of the neurons in the output layer. This list must be passed as an argument to the neuron: `connect_output/2` function to be connected to an output.
Be careful, only connect your outputs once the network has completed its workout, otherwise your output process will be overwhelmed with message.

## Network interaction

To send information to the network, it is necessary to use an external process connected to the input layer of the network and to send it the information to be transmitted. The information will then be transmitted to all the neurons of the input layer.

If the network was created using the `make_network function`, the list of PIDs of the processes used as input is returned in the value `Input_list` (see above)

If the network has been initialized manually, you must manually create the entries.
Simply create a process and make it use the neuron function: `input/3`.

```erlang
Input1 = spawn(fun () -> neuron:input(2, 1, C2) end),
Input2 = spawn(fun () -> neuron:input(2, 2, C2) end).
```

To send a value to an entry it is enough to send the message `{input, Value}` to the process created. For example, to send the value 1 to the input Input1 created previously: `Input1 ! {Input, 1}`.

### Retrieve the output values of the network

Once these computation are complete, the network will send these results to the specified processes. (These processes are specified at creation if you created the neuron manually or by using the `neuron:connect_output/2` function if you have initialized an empty network). The sent message is: `{done, Result, {PID, Layer, Rank}}` with:

- `Result` : the result of the computation ; 
- `PID` : the PID of the neuron who's send the result ; 
- `Layer` : The rank of the layer who's send the result ; 
- `Rank` : the rank of the sender.

## Neuron training

To create the supervisor we will reuse the values returned by the function: `neuron:make_network`.
```erlang
Trainer = trainer:init(Network, Network_size, Input_list).
```

`Trainer` is the PID of the supervisor.

### launch a training
