INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)

COMMON_SOURCES = 		\
	cpg-network.c \
	cpg-object.c \
	cpg-state.c \
	cpg-relay.c \
	cpg-link.c \
	cpg-property.c \
	cpg-tokenizer.c \
	cpg-expression.c \
	cpg-enum-types.c \
	cpg-ref-counted.c \
	cpg-monitor.c \
	cpg-math.c \
	cpg-debug.c \
	cpg-stack.c \
	cpg-network-reader.c \
	cpg-network-writer.c \
	cpg-compile-error.c

NOINST_H_FILES = \
	cpg-tokenizer.h \
	cpg-network-reader.h \
	cpg-network-writer.h \
	cpg-ref-counted-private.h \
	cpg-utils.h

INST_H_FILES = \
	cpg-network.h \
	cpg-object.h \
	cpg-state.h \
	cpg-relay.h \
	cpg-link.h \
	cpg-monitor.h \
	cpg-property.h \
	cpg-expression.h \
	cpg-ref-counted.h \
	cpg-stack.h \
	cpg-debug.h \
	cpg-compile-error.h \
	cpg-math.h

lib_LTLIBRARIES = libcpg-network-1.0.la

libcpg_network_headers = \
	$(INST_H_FILES) \
	cpg-enum-types.h

cpg-enum-types.h: cpg-enum-types.h.template $(INST_H_FILES) $(GLIB_MKENUMS)
	(cd $(srcdir) && $(GLIB_MKENUMS) --template cpg-enum-types.h.template $(INST_H_FILES)) > $@

cpg-enum-types.c: cpg-enum-types.c.template cpg-enum-types.h $(INST_H_FILES) $(GLIB_MKENUMS)
	(cd $(srcdir) && $(GLIB_MKENUMS) --template cpg-enum-types.c.template $(INST_H_FILES)) > $@

libcpg_network_1_0_la_CFLAGS = -D_GNU_SOURCE -Wall $(CPG_NETWORK_CFLAGS)
libcpg_network_1_0_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*" $(CPG_NETWORK_LIBS) -lm
libcpg_network_1_0_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network

libcpg_network_1_0_la_SOURCES = $(COMMON_SOURCES)

libcpg_network_1_0_include_HEADERS = $(libcpg_network_headers)

CLEANFILES = cpg-enum-types.c cpg-enum-types.h

EXTRA_DIST = \
	$(NOINST_H_FILES) \
	cpg-enum-types.c.template \
	cpg-enum-types.h.template
