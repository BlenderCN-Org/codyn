AM_CPPFLAGS =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	-I$(top_srcdir)/codyn		\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"

noinst_LTLIBRARIES = libtreealgorithms.la

libtreealgorithms_la_SOURCES = 	\
	cdn-tree-algorithms-canonicalize.c \
	cdn-tree-algorithms-linsolve.c \
	cdn-tree-algorithms-private.c \
	cdn-tree-algorithms-simplify.c \
	cdn-tree-algorithms-derive.c \
	cdn-tree-algorithms-string.c \
	cdn-tree-algorithms-substitute.c

NOINSTHEADERS = \
	cdn-tree-algorithms-private.h

libtreealgorithms_la_CFLAGS = -D_GNU_SOURCE -Wall $(CODYN_CFLAGS)

EXTRA_DIST = $(NOINSTHEADERS)

-include $(top_srcdir)/git.mk
