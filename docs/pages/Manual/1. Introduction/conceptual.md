#<cldoc:Manual::1. Introduction::1. Conceptual Overview>

Concepts and terminology.

Codyn has been designed around concepts which give an intuitive notion to
building coupled dynamics system. In codyn, a coupled dynamical system is called a
network. The network term arises from the fact that in codyn, dynamical systems
are modeled using *nodes* and *edges*, which are structurally organized as
a network.

## Nodes
A node is simply a container for variables. It can contain both *state*
variables and *normal* variables. Additionally nodes can be embedded into other nodes,
creating a hierarchical organization of the network. Embedded nodes have
direct access the variables of their parent nodes, but not of their siblings.

## Edges
An edge connects two nodes and defines a coupling of variables
between nodes. For *state* variables, an edge defines a differential term of
the state variable. Edges can reference variables from both input and output
nodes and a single edge can define coupling equations for more than one variable.

It's important to realize that *all* differential equations in codyn are
implemented using *edges*. To make writing differential equations which only
access states and variables from the same *node* more convenient, each *node*
also contains a so-called *self-edge*. The *self-edge* is basically an *edge*
with its *input* and *output* set to the node it is contained within.

## Network
![centered][concept]

A network in codyn is a special kind of top-level node (i.e. it does not have
a parent node) and represents your complete dynamical system. You can define
states and variables on the network like any other node. Networks also define
which type of integrator should be used to numerically integrate the system.

The figure above shows a very simple network of two *nodes* and two *edges*.
In this example network, the nodes could be oscillators which are bidirectionally
coupled. A network can be programmatically constructed using libcodyn, or defined
in a plain text file using the codyn language. The following code would generate
a network similar to the one depicted in the figure above.

<<<cdn/conceptual_network.cdn>>>

This would define a network with two nodes, each implementing a simple amplitude
controlled phase oscillator with phase state [**θ**] and amplitude state [**r**].
A bidirectional edge between the nodes adds a coupling
term on the phase variable such that the oscillators will synchronize at a quarter
pi phase difference. The details of the codyn language are
discussed in detail in <Manual::3. Modeling>.

## Templates
It is very common that you want to build a network out of nodes which share
the same functionality, variables or states. To support this, codyn has the
concept of *templates*. Templates are simply nodes and edges which can be
conveniently used to instantiate a new object which inherits all of the template
functionalities. Templates are defined in a special template section of the
network and are not simulated themselves. Looking again at the simple example
network defined in the previous section, we can rewrite the example using
templates as follows.

<<<cdn/conceptual_network_templates.cdn>>>

We can now reuse the *oscillator* template as many times as we want. Any
changes made to the template are inherited by the instances of the template.
You can also put your templates in a separate file to build a library which you
can then include in your networks.

libcodyn installs a number of these type of library files containing examples
of oscillators and couplings.

## Simulation
codyn provides a number of ways to simulate (numerically integrate) your network
after you have it constructed. You can open your network in the
<Manual::4. Tools::studio> to run simulations interactively while plotting
variables. Alternatively, you can use the <Manual::4. Tools::Command line> tool
*cdn-monitor* to run a simulation from the terminal. This tool will simply output
the values of the variables you specify to monitor. Finally, you can open
the network programmatically in a number of supported programming languages
(see <Manual::5. Bindings>).

[concept]: figures/concept.png
