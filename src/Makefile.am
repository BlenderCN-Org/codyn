INCLUDES =				\
	-I$(srcdir)

COMMON_SOURCES = 		\
	cpg-network.c \
	cpg-object.c \
	cpg-state.c \
	cpg-link.c \
	cpg-property.c \
	cpg-tokenizer.c \
	cpg-expression.c \
	cpg-math.c \
	cpg-debug.c

NOINST_H_FILES = \
	cpg-tokenizer.h \
	cpg-utils.h \
	cpg-math.h

lib_LTLIBRARIES = libcpg-network-0.1.la
libcpg_network_headers = \
	cpg-network.h \
	cpg-object.h \
	cpg-state.h \
	cpg-link.h \
	cpg-property.h \
	cpg-expression.h \
	cpg-debug.h

libcpg_network_0_1_la_SOURCES = $(COMMON_SOURCES)

libcpg_network_0_1_la_CFLAGS = -D_GNU_SOURCE
libcpg_network_0_1_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*"
libcpg_network_0_1_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network

libcpg_network_0_1_include_HEADERS = $(libcpg_network_headers)

EXTRA_DIST = $(NOINST_H_FILES)
