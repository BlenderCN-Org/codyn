INCLUDES =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_srcdir)		\
	$(CPG_NETWORK_CFLAGS)

bin_PROGRAMS = cpg-compile

cpg_compile_SOURCES = \
	cpg-compile.c

cpg_compile_LDADD = $(top_builddir)/cpg-network/libcpg-network-$(CPG_NETWORK_API_VERSION).la -lm $(CPG_NETWORK_LIBS) $(CPG_PARSER_LIBS)

-include $(top_srcdir)/git.mk
