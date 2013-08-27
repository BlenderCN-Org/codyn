AM_CPPFLAGS =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_builddir)/tests	\
	-I$(top_srcdir)/tests	\
	-I$(top_srcdir)		\
	$(CODYN_CFLAGS)

bin_PROGRAMS = cdn-archive

cdn_archive_SOURCES = \
	cdn-archive.c

cdn_archive_LDADD = $(top_builddir)/codyn/libcodyn-$(CODYN_API_VERSION).la -lm $(CODYN_LIBS) $(CDN_ARCHIVE_LIBS)

-include $(top_srcdir)/git.mk
