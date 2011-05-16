INCLUDES =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_builddir)/tests	\
	-I$(top_srcdir)/tests	\
	-I$(top_srcdir)		\
	$(CPG_NETWORK_CFLAGS)

bin_PROGRAMS = cpg-parser

cpg_parser_SOURCES = \
	cpg-parser.c

cpg_parser_LDADD = $(top_builddir)/cpg-network/libcpg-network-$(CPG_NETWORK_API_VERSION).la -lm $(CPG_NETWORK_LIBS) $(CPG_PARSER_LIBS)

-include $(top_srcdir)/git.mk
