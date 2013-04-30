plugindir = $(IO_LIBS_DIR)

AM_CPPFLAGS =                           \
	-I$(srcdir)                     \
	-I$(top_srcdir)                 \
	-DDATADIR=\""$(datadir)"\"      \
	-DLIBDIR=\""$(libdir)"\"

plugin_LTLIBRARIES = libnetwork.la

hfiles = 				\
	cdn-client.h			\
	cdn-network-thread.h		\
	cdn-io-network-server.h		\
	cdn-io-network-client.h		\
	cdn-io-network.h

NOINST_H_FILES = 			\
	$(hfiles)			\
	cdn-io-network-enum-types.h

libnetwork_la_SOURCES =			\
	cdn-io-network-enum-types.c	\
	cdn-client.c			\
	cdn-io-network-client.c		\
	cdn-io-network-server.c		\
	cdn-network-thread.c		\
	cdn-io-network.c		\
	$(NOINST_H_FILES)

libnetwork_la_LDFLAGS = $(IO_LIBTOOL_FLAGS)
libnetwork_la_LIBADD = $(CODYN_LIBS) -lm
libnetwork_la_CFLAGS = $(CODYN_CFLAGS)

cdn-io-network-enum-types.h: cdn-io-network-enum-types.h.template $(hfiles) $(GLIB_MKENUMS)
	(cd $(srcdir) && $(GLIB_MKENUMS) --template cdn-io-network-enum-types.h.template $(hfiles)) > $@

cdn-io-network-enum-types.c: cdn-io-network-enum-types.c.template cdn-io-network-enum-types.h $(hfiles) $(GLIB_MKENUMS)
	(cd $(srcdir) && $(GLIB_MKENUMS) --template cdn-io-network-enum-types.c.template $(hfiles)) > $@

libnetwork_la-cdn-io-network-enum-types.lo: cdn-io-network-enum-types.h

install-data-hook:
	rm -f $(DESTDIR)$(plugindir)/$(plugin_LTLIBRARIES)

uninstall-hook:
	rm -f $(DESTDIR)$(plugindir)/$(plugin_LTLIBRARIES:.la=.so)

EXTRA_DIST =					\
	cdn-io-network-enum-types.c.template	\
	cdn-io-network-enum-types.h.template	\
	$(NOINST_H_FILES)

CLEANFILES =					\
	cdn-io-network-enum-types.c		\
	cdn-io-network-enum-types.h

-include $(top_srcdir)/git.mk
