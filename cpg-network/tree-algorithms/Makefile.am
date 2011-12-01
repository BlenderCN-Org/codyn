INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	-I$(top_srcdir)/cpg-network	\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"

noinst_LTLIBRARIES = libtreealgorithms.la

libtreealgorithms_la_SOURCES = 	\
	cpg-tree-algorithms-canonicalize.c \
	cpg-tree-algorithms-linsolve.c \
	cpg-tree-algorithms-private.c \
	cpg-tree-algorithms-simplify.c \
	cpg-tree-algorithms-string.c \
	cpg-tree-algorithms-substitute.c

NOINSTHEADERS = \
	cpg-tree-algorithms-private.h

libtreealgorithms_la_CFLAGS = -D_GNU_SOURCE -Wall $(CPG_NETWORK_CFLAGS)

EXTRA_DIST = $(NOINSTHEADERS)

-include $(top_srcdir)/git.mk
