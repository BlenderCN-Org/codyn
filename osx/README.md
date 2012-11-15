OS X Framework build instructions
=================================

1. Make sure to update the brew git submodule
   git submodule init osx/brew
   git submodule update osx/brew

2. Bootstrap the dependencies with brew
   ./bootstrap

   This will compile and install the necessary dependencies in a local
   brew instance (in osx/brew)

3. Build codyn
   ./build

   This will compile codyn with the correct flags to make a universal
   dynamic library suitable for inclusion in an OS X framework

4. Make the framework
   ./mkfw

   This little script simply copies the needed libraries, headers and other
   resources to an OS X framework structured directory. It also fixes library
   link paths such that they correctly point to /System/Library/Frameworks/Codyn.framework.
   All the dependencies compiled with brew in step 2 are included in the
   framework.

After these steps have completed successfully, a Codyn.framework directory should
be present in the osx directory. This framework can be copied to
/System/Library/Frameworks/ and then used as is.
