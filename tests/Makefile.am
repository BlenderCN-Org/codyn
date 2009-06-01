INCLUDES =				\
	-I$(srcdir)			\
	-I$(builddir)			\
	-I$(top_builddir)/tests		\
	-I$(top_srcdir)/tests		\
	-I$(top_srcdir)				\
	$(CPG_NETWORK_CFLAGS)

EXTRA_PROGRAMS = expression test

noinst_PROGRAMS = expression test
expression_LDADD = $(top_builddir)/cpg-network/libcpg-network-$(CPG_NETWORK_API_VERSION).la -lm $(CPG_NETWORK_LIBS)

expression_SOURCES = \
	expression.c \
	utils.c \
	utils.h

test_LDADD = $(top_builddir)/cpg-network/libcpg-network-$(CPG_NETWORK_API_VERSION).la -lm $(CPG_NETWORK_LIBS)

test_SOURCES = \
	test.c
