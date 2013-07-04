#<cldoc:Manual::1. Introduction::1. Conceptual Overview>

Concepts and terminology.

Codyn has been designed around concepts which give an intuitive notion to
building coupled dynamics system. In codyn, a coupled dynamical system is called a
network. The network term arises from the fact that in codyn, dynamical systems
are modeled using *nodes* and *edges*, which are structurally organized as
a network.

# 1. Node
A node is simply a container for variables. It can contain both *state*
variables and *normal* variables. Additionally nodes can be embedded into other nodes,
creating a hierarchical organization of the network. Embedded nodes have
direct access the variables of their parent nodes, but not of their siblings.

# 2. Edge
An edge connects two nodes and defines a coupling of variables
between nodes. For *state* variables, an edge defines a differential term of
the state variable. Edges can reference variables from both input and output
nodes and a single edge can define coupling equations for more than one variable.

# 3. Network
![centered][concept]

A network in codyn is a special kind of top-level node (i.e. it does not have
a parent node).

# 4. Templates

[concept]: figures/concept.png
