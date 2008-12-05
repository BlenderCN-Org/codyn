INCLUDES =				\
	-I$(srcdir)			\
	-I$(builddir)			\
	-I$(top_builddir)/tests		\
	-I$(top_srcdir)/tests		\
	-I$(top_srcdir)/src

EXTRA_PROGRAMS = expression

noinst_PROGRAMS = expression
expression_LDADD = $(top_builddir)/src/libcpg-network-$(CPG_NETWORK_API_VERSION).la -lm

expression_SOURCES = \
	expression.c \
	utils.c \
	utils.h
