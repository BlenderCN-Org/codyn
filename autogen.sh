#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="cpg-network"

(test -f $srcdir/configure.ac \
  && test -f $srcdir/README \
  && test -d $srcdir/cpg-network) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level $PKG_NAME directory"
    exit 1
}

aclocal && autoconf && automake --add-missing
