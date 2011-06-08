INCLUDES =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_srcdir)		\
	$(CPG_NETWORK_CFLAGS)

bin_PROGRAMS = cpg-render

cpg_render_SOURCES = \
	cpg-render.c

EXTRA_DIST = $(NOINST_H_FILES)

cpg_render_LDADD = $(top_builddir)/cpg-network/libcpg-network-$(CPG_NETWORK_API_VERSION).la -lm $(CPG_NETWORK_LIBS)

-include $(top_srcdir)/git.mk
