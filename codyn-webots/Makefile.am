INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)

COMMON_SOURCES = 		\
	codyn-webots.c

lib_LTLIBRARIES = libcodyn-webots-2.0.la
libcodyn_webots_headers = \
	codyn-webots.h

NOINST_H_FILES =

libcodyn_webots_2_0_la_CFLAGS = -D_GNU_SOURCE -Wall $(WEBOTS_CFLAGS) $(CODYN_CFLAGS)
libcodyn_webots_2_0_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*" $(CODYN_LIBS) $(WEBOTS_LIBS) -L$(top_builddir)/codyn -lcodyn-2.0
libcodyn_webots_2_0_includedir = $(includedir)/codyn-$(CODYN_API_VERSION)/codyn-webots

libcodyn_webots_2_0_la_SOURCES = $(COMMON_SOURCES)
libcodyn_webots_2_0_include_HEADERS = $(libcodyn_webots_headers)

EXTRA_DIST = \
	$(NOINST_H_FILES)

-include $(top_srcdir)/git.mk
