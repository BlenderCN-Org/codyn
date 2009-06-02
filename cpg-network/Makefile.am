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
	cpg-ref-counted.c \
	cpg-monitor.c \
	cpg-math.c \
	cpg-debug.c \
	cpg-stack.c \
	cpg-network-reader.c \
	cpg-enum-types.c \
	cpg-compile-error.c

NOINST_H_FILES = \
	cpg-tokenizer.h \
	cpg-network-reader.h \
	cpg-math.h

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
	cpg-types.h \
	cpg-monitor.h \
	cpg-compile-error.h
	
lib_LTLIBRARIES = libcpg-network-1.0.la

libcpg_network_headers = \
	$(INST_H_FILES) \
	cpg-enum-types.h

cpg-enum-types.h: cpg-enum-types.h.template $(INST_H_FILES) $(GLIB_MKENUMS)
	(cd $(srcdir) && $(GLIB_MKENUMS) --template cpg-enum-types.h.template $(INST_H_FILES)) > $@
	
cpg-enum-types.c: cpg-enum-types.c.template $(INST_H_FILES) $(GLIB_MKENUMS)
	(cd $(srcdir) && $(GLIB_MKENUMS) --template cpg-enum-types.c.template $(INST_H_FILES)) > $@

libcpg_network_1_0_la_CFLAGS = -D_GNU_SOURCE -Wall -Werror $(CPG_NETWORK_CFLAGS)
libcpg_network_1_0_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*" $(CPG_NETWORK_LIBS)
libcpg_network_1_0_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network


sharedincdir = $(libcpg_network_1_0_includedir)/shared
sharedinc_DATA = \
	shared/cpg-shared-expression.h \
	shared/cpg-shared-property.h \
	shared/cpg-shared-network.h \
	shared/cpg-shared-utils.h \
	shared/cpg-shared-link.h
	
libcpg_network_1_0_la_SOURCES = $(COMMON_SOURCES)

libcpg_network_1_0_include_HEADERS = $(libcpg_network_headers)

if ENABLE_RT
lib_LTLIBRARIES += libcpg-network-rt-0.1.la

libcpg_network_rt_1_0_la_CFLAGS = -D_GNU_SOURCE -Wall -Werror -DRTLINUX $(RT_CFLAGS)
libcpg_network_rt_1_0_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*"
libcpg_network_rt_1_0_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network

libcpg_network_rt_1_0_la_SOURCES = \
	shared/cpg-shared-network.c \
	shared/cpg-shared-property.c \
	shared/cpg-shared-expression.c \
	cpg-stack.c \
	cpg-math.c \
	cpg-debug.c
	
libcpg_network_rt_1_0_include_HEADERS = \
	shared/cpg-shared-network.h \
	shared/cpg-shared-expression.h \
	shared/cpg-shared-property.h \
	shared/cpg-shared-utils.h \
	shared/cpg-shared-link.h \
	cpg-debug.h
endif

EXTRA_DIST = \
	$(NOINST_H_FILES) \
	$(sharedinc_DATA) \
	cpg-enum-types.c.template \
	cpg-enum-types.h.template
