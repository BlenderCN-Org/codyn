INCLUDES =				\
	-I$(srcdir)			\
	-I$(builddir)			\
	-I$(top_builddir)/tests		\
	-I$(top_srcdir)/tests		\
	-I$(top_srcdir)				\
	$(CPG_NETWORK_CFLAGS)

TEST_PROGS = expression
progs_ldadd = $(top_builddir)/cpg-network/libcpg-network-$(CPG_NETWORK_API_VERSION).la

noinst_PROGRAMS = $(TEST_PROGS)

expression_SOURCES = expression.c utils.h
expression_LDADD = $(progs_ldadd)

TESTS = $(TEST_PROGS)

-include $(top_srcdir)/git.mk
