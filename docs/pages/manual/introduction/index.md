#<cldoc:Manual::Introduction>

Introduction to the codyn framework.

Codyn is a general purpose open source software framework for modeling and
simulating **Co**upled **Dyn**amical Systems. It uses object oriented concepts to
define coupled systems in a concise manner using a special purpose modeling
language allowing the user to directly define dynamical systems.

Codyn has been designed as a core C library with an elegant public API, and a set of tools built on top of this C library. This approach ensures that the the core C library exposes all relevant API to built your own external tools, if needed. Furthermore, the C library can be easily expanded externally to add new integrator schemes, new io methods and additional mathematical functions. The core C library is furthermore built on top of GObject which provides a number of advantages (such as automatic bindings for various programming languages, including Python and automatic documentation generation).

A graphical user interface is provided to design, manipulate and test dynamical systems easily. States can be easily monitored and plotted to investigate the resulting dynamics. This application is particularly useful to play with parameters and designs of your system in an interactive manner.

The core codyn library features a small byte code compiler and corresponding interpreter which converts high level representations of the dynamical equations into instructions which can be evaluated. One of the various numerical integrator schemes that are built into the library can then integrate the system. This approach allows you to model dynamical systems in simple text files which can be integrated without any further compilation.

For applications where high performance is required, or for systems with very
strict requirements, the high level description of the dynamics can be compiled
to a very low-level C implementation. The generated code does not have any
dependencies and can be compiled for a large variety of platforms (including microcontrollers).
See <Manual::Tools::rawc> for more information.

#<cldoc:include(conceptual.md)>
#<cldoc:include(firstnetwork.md)>
#<cldoc:include(usingstudio.md)>
#<cldoc:include(performance.md)>
#<cldoc:include(physics.md)>
