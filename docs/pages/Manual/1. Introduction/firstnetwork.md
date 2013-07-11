#<cldoc:Manual::1. Introduction::2. First Network>

Writing your first network.

If you've been reading the <[conceptual overview]1. Conceptual Overview>, then you have already
seen how a very simple example network can be defined. Here we will go
through constructing and simulating a network step by step so that you can
get an intuition of how you can use codyn to simulate dynamical networks.

## The system
Although networks can be created completely programmatically, the canonical and
convenient way to create a network is by using the codyn modeling language.
We will not go fully into the details of the language here, but provide a
short introduction into using its basic features. For more details on the
modeling language see <[modeling language]3. Modeling>.

The goal of this tutorial is to create the oscillator network as presented
in [From Swimming to Walking with a Salamander Robot Driven by a Spinal Cord Model (Ijspeert et al, 2007)][science2007]. In this article, a network of simple amplitude controlled phase
oscillator are used as an abstract representation of the salamander spinal
cord circuitry responsible for generating both swimming and walking.

![centered][scienceeq]

The equations above are used to model one oscillator in the
dynamical system of the salamander spinal cord (Ijspeert et al., 2007). Note
that each oscillator receives coupling on `θ` from its neighbors. Also,
the differential equation for `r` is second order and we will see how we
can implement this in codyn. In the article, a double chain of such oscillators
represent the full spinal chord. Although there are additional oscillators driving
the limbs and a mechanism to switch between swimming and walking, here we will
only model the swimming part of the system.

[science2007]: http://www.sciencemag.org/content/315/5817/1416.short
[scienceeq]: figures/scienceeq.png

## Base oscillator

Let's start by constructing the base amplitude controlled phase oscillator node.
We will model the phase and amplitude evolution of the oscillator, but not the
coupling term.

<<<cdn/first_network/oscillator.cdn>>>

Here we have defined an *oscillator* node in the templates section of the
network. The *oscillator* defines the variables `R`, `a` and `ν` wich
can be used to control the amplitude, rate of convergence and frequency of the
oscillator. It then defines two differential equations for the phase `θ`
and amplitude `r` using the *prime* syntax.

The *prime* syntax (e.g. `θ'`) can be used to define a state variable and
its corresponding differential equation on a single line. The initial condition
for the state variable will always be `0`, but can be defined separately to
a different value later. Furthermore, if you look closely at the differential
equation of `r` in the code above, you'll notice that we've used **two**
primes! This is a shorthand syntax to generate a second order differential
equation. Codyn internally only supports first order differential equations and
rewrites the order N equation as a cascade of first order equations. The
convenience syntax is equivalent to the following:

```cdn
 r' = "dr"
dr' = "a ∙ (a / 4 * (R - r) - dr)"
```

By using `r''`, the `dr` variable will be automatically added by codyn. Note
also that we can refer to the differential equation of `r` inside the
differential equation of `dr` by writing `r'`.

## Coupling
Next we will define the basic coupling between two oscillators as an edge
in the template section.

<<<cdn/first_network/coupling.cdn>>>

The edge defines a single additive term (edges always define additive terms)
to the differential equation of the phase variable `θ'`. This type of coupling
is diffusive and can be seen as a forcing term towards a certain phase difference
between two oscillators, called the phase bias `Φ`. Note that there is a context
in which variables in equations are resolved. For an edge, a variable is first
looked for in the edge itself (for example `w`). If not found, the variable is
then looked for in the *input* node of the edge. Thus, in the above definition
of coupling differential equation for `θ`, `r` refers to the
amplitude state variable of the *input* node of the *coupling* edge. Finally,
you can also refer explicitly to variables in either *input* or *output* nodes
of an edge by using `input.variable` or `output.variable`. An example of this
is `output.θ` to access the phase state variable of the *output* node of the
coupling edge.

## Generating the nodes
Now that we have defined the `oscillator` and `coupling` templates, we can
continue generating the actual nodes that will represent our chain of oscillators.

<<<cdn/first_network/chain.cdn>>>

There are a couple of new things that we used here to define the nodes. First of
all, we used something called `defines`. Defines are a language level type of
variable that you can use to parametrize the generation of your network. In
this case, we use a define to set the number of oscillators that we want
to generate. By doing this, we can later very easily change the number of
oscillators in the chain by only changing the value of `n`.
After having defined `n` we can refer to its value
anywhere in our network using `@n`. Note that we use
the special `?=` (instead of just `=`) to define the value of `n`. This means
that `n` will only be defined if it was not defined before. Doing so allows you
to include the above file somewhere else with a different defined `n`. For example:

```cdn
defines { n = 10 } include "first_network.cdn"
```

To generate the actual nodes representing
the oscillators, we use the generator syntax of the modeling language. This
syntax works very similar to brace expansion in `bash`. The braces generate
ranges of values which are expanded inside the string they occur in. For example
`"{a,b,c}"` would expand to `"a"`, `"b"` and `"c"`. Brace expansion is also
combinatorial, meaning that `"{a,b}_{c,d}"` would expand to `"a_c"`, `"a_d"`,
`"b_c"` and `"b_d"`. Additionally, `"n{1:3}"` would generate `"n1"`, `"n2"`
and `"n3"`. Using this syntax, we can easily generate the chain of oscillators
as done in the code shown above.

Finally, we use one line to position the generated oscillator nodes on a grid.
This type of layouting is only used for visualizing the network and has no
effect on the simulation. To layout all the nodes along a chain we use
<[embedded calculations]Manual::3. Modeling::Embedded calculations> and
<[expansion references]Manual::3. Modeling::Expansion references>.

Calculations
can be done inside the model specification using the `$(expression)` syntax.
The `expression` will be evaluated (after expanding references) and injected
at the point of the embedded calculation.

Expansion references can be used to refer to previously expanded expressions.
The syntax for these references is `@N` with `N` being a number. `@0` always
refers to the full expansion, while `@1`, `@2` etc. refer to sub-groups of
the expansion. In the case of brace expansions, `@1` would refer to the expansion
of the first occurence of braces. The special expansion reference `@N[!]` refers
to the *index* of expansion `N`. When refering to brace expansions, it's the
*index* of the expanded value. For example, the brace expansion `"n_{a,b}"` would
generate the following expansion references:

| Reference | value 1| value 2 |
|-----------|:------:|:-------:|
| `@0`      | n\_a   | n\_b    |
| `@1`      | a      | b       |
| `@1[!]`   | 0      | 1       |

## Generating the couplings

We have previously generated all the oscillator nodes, now we only need to
couple the phases of these oscillators such that they are producing a traveling
wave. We do this by introducing coupling edges between successive nodes on
each side of the chain (right and left). Furthermore, each pair of right/left
oscillators should oscillate in anti-phase. We therefore also introduce ipsilateral
coupling for each oscillator.

<<<cdn/first_network/couplings.cdn>>>

Generating the edges is fairly straightforward. We make use of one new feature
here to automatically generate `bidirectional` coupling. This means that whenever
an edge is constructed with some `input` and `output`, another edge is constructed
automatically in the other way around. The phase coupling that we want to
introduce here should be symmetric. This basically means that we need to
negate the phase bias `Φ` in the bidirectional direction. To do this, we use
a feature called <[multi expressions]Manual::3. Modeling::Multi expressions>.
Multi expressions allow you to specify different expressions for multiple objects
generated at the same level. The syntax is `["expression 1", "expression 2", "etc."]`.
This expression would evaluate to `"expression 1"` for the first generated object,
`"expression 2"` for the second generated object etc.

In our couplings we use this to set the expression of the `d` variable to
`1` and `-1` for respectively the up-to-down and down-to-up couplings (and
similarly for left-to-right and right-to-left).

## Complete network
The following code shows the complete working network with annotations.

<<<cdn/first_network/complete.cdn>>>

When visualized, this network looks like the following figure:

![centered][complete]

[complete]: figures/complete.png


