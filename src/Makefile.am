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
	cpg-debug.c \
	cpg-stack.c \
	cpg-thread.c \
	cpg-mutex.c \
	cpg-shared-network.c \
	shared/cpg-shared-expression.c \
	shared/cpg-shared-property.c

NOINST_H_FILES = \
	cpg-tokenizer.h \
	cpg-utils.h \
	cpg-math.h \
	cpg-link-private.h \
	cpg-object-private.h \
	cpg-expression-private.h \
	cpg-state-private.h \
	cpg-network-private.h \
	cpg-thread.h \
	cpg-mutex.h

lib_LTLIBRARIES = libcpg-network-0.1.la
libcpg_network_headers = \
	cpg-network.h \
	cpg-object.h \
	cpg-state.h \
	cpg-link.h \
	cpg-property.h \
	cpg-expression.h \
	cpg-stack.h \
	cpg-debug.h \
	cpg-types.h \
	cpg-shared-network.h \
	shared/cpg-shared-expression.h \
	shared/cpg-shared-property.h \
	shared/cpg-shared-network.h \
	shared/cpg-shared-utils.h \
	shared/cpg-shared-link.h

libcpg_network_0_1_la_CFLAGS = -D_GNU_SOURCE -Wall -Werror
libcpg_network_0_1_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*"
libcpg_network_0_1_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network

if ENABLE_THREADS
libcpg_network_0_1_la_CFLAGS += $(THREAD_CFLAGS)
libcpg_network_0_1_la_LDFLAGS += $(THREAD_LIBS)
endif

libcpg_network_0_1_la_SOURCES = $(COMMON_SOURCES)

libcpg_network_0_1_include_HEADERS = $(libcpg_network_headers)

if ENABLE_RT
lib_LTLIBRARIES += libcpg-network-rt-0.1.la

libcpg_network_rt_0_1_la_CFLAGS = -D_GNU_SOURCE -Wall -Werror -DRTLINUX
libcpg_network_rt_0_1_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*"
libcpg_network_rt_0_1_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network

libcpg_network_rt_0_1_la_SOURCES = \
	shared/cpg-shared-network.c \
	shared/cpg-shared-property.c \
	shared/cpg-shared-expression.c \
	cpg-stack.c \
	cpg-math.c \
	cpg-debug.c
	
libcpg_network_rt_0_1_include_HEADERS = \
	shared/cpg-shared-network.h \
	shared/cpg-shared-expression.h \
	shared/cpg-shared-property.h \
	shared/cpg-shared-utils.h \
	shared/cpg-shared-link.h \
	cpg-debug.h
endif

EXTRA_DIST = $(NOINST_H_FILES)
