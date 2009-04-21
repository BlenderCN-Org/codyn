INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)

COMMON_SOURCES = 		\
	cpg-network-webots.c

lib_LTLIBRARIES = libcpg-network-webots-1.0.la
libcpg_network_webots_headers = \
	cpg-network-webots.h

NOINST_H_FILES = \
	cpg-network-webots-private.h

libcpg_network_webots_1_0_la_CFLAGS = -D_GNU_SOURCE -Wall -Werror $(WEBOTS_CFLAGS) $(CPG_NETWORK_CFLAGS)
libcpg_network_webots_1_0_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*"
libcpg_network_webots_1_0_la_LIBS = $(CPG_NETWORK_LIBS)
libcpg_network_webots_1_0_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network-webots

libcpg_network_webots_1_0_la_SOURCES = $(COMMON_SOURCES)
libcpg_network_webots_1_0_include_HEADERS = $(libcpg_network_webots_headers)

EXTRA_DIST = \
	$(NOINST_H_FILES)
