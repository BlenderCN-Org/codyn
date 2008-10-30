INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)/src

EXTRA_PROGRAMS = expression

noinst_PROGRAMS = expression
expression_LDADD = $(top_srcdir)/src/libcpg-network-$(CPG_NETWORK_API_VERSION).la -lm

expression_SOURCES = \
	expression.c \
	utils.c
