#<cldoc:Manual::Introduction::Using the studio>

How to use the codyn studio.

The codyn studio is a graphical user interface which can be used to inspect
and design networks visually. It also allows you to simulate the network and
plot variables over time. It is easy to investigate how changes of some
variables influence your dynamics.

## Loading a network
To load a network, simply start the studio and use File/Open to open your
network. Here we open the example developed in the
<[previous chapter]First Network>.

![centered][studio_complete]

You can see the basic oscillators and their couplings of the double chain
on the canvas (central area). The bottom area shows the variables of the
currently selected node and allows modifying its values.

**Note**: it's important to understand that although the studio can *load*
a network written in the modeling language, it **cannot** *save* it back. So
although the studio is useful in this case for plotting variables and investigating
the influence of certain variable values, you should not use it to design
your network.

## Plotting variables

If you select the first four oscillators on the left, right click to get the
context menu, and select Monitor/x, then you should see something like the
following dialog plotting the `x` values of each of the selected oscillators:

![centered][studio_complete_monitor]

You can see that the radius of each oscillator slowly converges to the desired
radius and that the phases are synchronizing to a constant phase difference
between consecutive oscillators.

[studio_complete]: figures/studio_complete.png
[studio_complete_monitor]: figures/studio_complete_monitor.png
