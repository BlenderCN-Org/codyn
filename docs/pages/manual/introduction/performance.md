#<cldoc:Manual::Introduction::Performance>

Compiling your network for high performance.

Although libcodyn has been optimized for running networks as fast as possible,
there is still a lot of overhead from having everything defined in a high-level
dynamic manner. All the mathematical expressions in codyn are evaluated using
a very simple (and non-optimized) interpreting virtual machine. In this
chapter we will show you how you can use the `cdn-rawc` command line tool
to generate a low-level implementation of your network which can run many
times faster than the high-level network using libcodyn.

## Timing the network
First we will have a look at how much time it takes to run a network using
libcodyn. For our tests, we will use the oscillator network used in the previous
chapters. To time the network, we will simulate it for 100 seconds using
`cdn-monitor` and measure how much time it takes to run using the `time`
command line application.

```
$ time cdn-monitor -t 0:0.001:100 network.cdn

real	0m8.526s
user	0m8.509s
sys	0m0.004s
```

As can be seen, it takes approximately 8.5 seconds to run this network for
100 simulated seconds. Numbers may vary depending on the machine you run the
simulation on. Also, using `time` to measure performance is not very accurate
but it suffices for our purposes.

## Using cdn-rawc to compile the network
To compile the network to a low-level representation, we can use `cdn-rawc`. The
easiest way to get a compiled version of the network is to run the following
command:

```
$ cdn-rawc --compile --shared network.cdn

[CC] .o/network_shared.o
[CC] .o/network_run_shared.o
[CC] libnetwork.so
```

This will create a shared library containing the compiled code representing
the specified network. If you want to have a look at the generated code,
then simply leave out the `--compile` flag.

## Timing the rawc network
The rawc generated shared library can be used directly with `cdn-monitor` to
simulate the dynamics. We can therefore run the same command as before, replacing
network.cdn with libnetwork.so (or libnetwork.dylib when on OS X).

```
$ time cdn-monitor -t 0:0.001:100 libnetwork.so

real	0m0.515s
user	0m0.508s
sys	0m0.000s
```

As shown in the time output, the network can be simulated much faster now. In
this particular case, the simulation runs around 16 times faster.
