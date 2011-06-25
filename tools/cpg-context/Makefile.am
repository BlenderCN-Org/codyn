INCLUDES =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_builddir)/tests	\
	-I$(top_srcdir)/tests	\
	-I$(top_srcdir)		\
	$(CPG_NETWORK_CFLAGS)

bin_PROGRAMS = cpg-context

cpg_context_SOURCES = \
	cpg-context.c

cpg_context_LDADD = $(top_builddir)/cpg-network/libcpg-network-$(CPG_NETWORK_API_VERSION).la -lm $(CPG_NETWORK_LIBS) $(CPG_PARSER_LIBS)

-include $(top_srcdir)/git.mk
