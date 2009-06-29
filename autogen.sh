#!/bin/sh
# Run this to generate all the initial makefiles, etc.

if [ ! -f gtk-doc.make ]; then
	gtkdocize
fi

autoreconf --install
