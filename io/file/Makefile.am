plugindir = $(IO_LIBS_DIR)

AM_CPPFLAGS =                           \
	-I$(srcdir)                     \
	-I$(top_srcdir)                 \
	-DDATADIR=\""$(datadir)"\"      \
	-DLIBDIR=\""$(libdir)"\"

plugin_LTLIBRARIES = libfile.la

NOINST_H_FILES = 			\
	cdn-io-file.h			\
	cdn-input-file.h		\
	cdn-output-file.h

libfile_la_SOURCES = 			\
	cdn-io-file.c			\
	cdn-input-file.c		\
	cdn-output-file.c

libfile_la_LDFLAGS = $(IO_LIBTOOL_FLAGS)
libfile_la_LIBADD = $(CODYN_LIBS) -lm
libfile_la_CFLAGS = $(CODYN_CFLAGS)

install-data-hook:
	rm -f $(DESTDIR)$(plugindir)/$(plugin_LTLIBRARIES)

uninstall-hook:
	rm -f $(DESTDIR)$(plugindir)/$(plugin_LTLIBRARIES:.la=.so)

EXTRA_DIST = $(NOINST_H_FILES)

-include $(top_srcdir)/git.mk
