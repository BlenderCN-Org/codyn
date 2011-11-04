INCLUDES =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_builddir)/tests	\
	-I$(top_srcdir)/tests	\
	-I$(top_srcdir)		\
	$(CPG_NETWORK_CFLAGS)

bin_PROGRAMS = cpg-archive

cpg_archive_SOURCES = \
	cpg-archive.c

cpg_archive_LDADD = $(top_builddir)/cpg-network/libcpg-network-$(CPG_NETWORK_API_VERSION).la -lm $(CPG_NETWORK_LIBS) $(CPG_ARCHIVE_LIBS)

-include $(top_srcdir)/git.mk
