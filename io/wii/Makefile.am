plugindir = $(IO_LIBS_DIR)

INCLUDES =                              \
	-I$(srcdir)                     \
	-I$(top_srcdir)                 \
	-DDATADIR=\""$(datadir)"\"      \
	-DLIBDIR=\""$(libdir)"\"

plugin_LTLIBRARIES = libwii.la

NOINST_H_FILES = 			\
	cdn-io-wii.h			\
	cdn-io-wii-common.h		\
	cdn-io-wii-register.h		\
	cdn-io-wii-manager.h

libwii_la_SOURCES = 			\
	cdn-io-wii.c			\
	cdn-io-wii-common.c		\
	cdn-io-wii-register.c		\
	cdn-io-wii-manager.c

libwii_la_LDFLAGS = $(IO_LIBTOOL_FLAGS)
libwii_la_LIBADD = $(CODYN_LIBS) $(CDN_IO_WII_LIBS) -lm
libwii_la_CFLAGS = $(CODYN_CFLAGS) $(CDN_IO_WII_CFLAGS)

install-data-hook:
	rm -f $(DESTDIR)$(plugindir)/$(plugin_LTLIBRARIES)

-include $(top_srcdir)/git.mk
