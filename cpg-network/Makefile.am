INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)

COMMON_SOURCES = 		\
	cpg-compile-context.c \
	cpg-compile-error.c \
	cpg-debug.c \
	cpg-enum-types.c \
	cpg-expression.c \
	cpg-function.c \
	cpg-function-polynomial.c \
	cpg-group.c \
	cpg-instructions.c \
	cpg-integrator.c \
	cpg-integrator-euler.c \
	cpg-integrator-predict-correct.c \
	cpg-integrator-runge-kutta.c \
	cpg-integrator-state.c \
	cpg-integrator-stub.c \
	cpg-integrators.c \
	cpg-import.c \
	cpg-import-alias.c \
	cpg-link.c \
	cpg-math.c \
	cpg-monitor.c \
	cpg-network.c \
	cpg-network-deserializer.c \
	cpg-network-serializer.c \
	cpg-object.c \
	cpg-property.c \
	cpg-ref-counted.c \
	cpg-stack.c \
	cpg-state.c \
	cpg-tokenizer.c

NOINST_H_FILES = \
	cpg-ref-counted-private.h \
	cpg-tokenizer.h \
	cpg-network-xml.h

INST_H_FILES = \
	cpg-compile-context.h \
	cpg-compile-error.h \
	cpg-debug.h \
	cpg-expression.h \
	cpg-function.h \
	cpg-function-polynomial.h \
	cpg-group.h \
	cpg-instructions.h \
	cpg-integrator-euler.h \
	cpg-integrator.h \
	cpg-integrator-predict-correct.h \
	cpg-integrator-runge-kutta.h \
	cpg-integrator-state.h \
	cpg-integrator-stub.h \
	cpg-integrators.h \
	cpg-import.h \
	cpg-import-alias.h \
	cpg-link.h \
	cpg-math.h \
	cpg-monitor.h \
	cpg-network.h \
	cpg-network-deserializer.h \
	cpg-network-serializer.h \
	cpg-object.h \
	cpg-property.h \
	cpg-ref-counted.h \
	cpg-stack.h \
	cpg-state.h \
	cpg-utils.h

lib_LTLIBRARIES = libcpg-network-2.0.la

libcpg_network_headers = \
	$(INST_H_FILES) \
	cpg-enum-types.h

cpg-enum-types.h: cpg-enum-types.h.template $(INST_H_FILES) $(GLIB_MKENUMS)
	(cd $(srcdir) && $(GLIB_MKENUMS) --template cpg-enum-types.h.template $(INST_H_FILES)) > $@

cpg-enum-types.c: cpg-enum-types.c.template cpg-enum-types.h $(INST_H_FILES) $(GLIB_MKENUMS)
	(cd $(srcdir) && $(GLIB_MKENUMS) --template cpg-enum-types.c.template $(INST_H_FILES)) > $@

libcpg_network_2_0_la_CFLAGS = -D_GNU_SOURCE -Wall $(CPG_NETWORK_CFLAGS)
libcpg_network_2_0_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*" $(CPG_NETWORK_LIBS) -lm
libcpg_network_2_0_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network

libcpg_network_2_0_la_SOURCES = $(COMMON_SOURCES)

libcpg_network_2_0_include_HEADERS = $(libcpg_network_headers)

CLEANFILES = cpg-enum-types.c cpg-enum-types.h

EXTRA_DIST = \
	$(NOINST_H_FILES) \
	cpg-enum-types.c.template \
	cpg-enum-types.h.template

-include $(top_srcdir)/git.mk
