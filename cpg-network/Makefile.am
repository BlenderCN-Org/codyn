INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"

COMMON_SOURCES = 		\
	cpg-compile-context.c \
	cpg-compile-error.c \
	cpg-debug.c \
	cpg-enum-types.c \
	cpg-marshal.c \
	cpg-expression.c \
	cpg-function.c \
	cpg-function-argument.c \
	cpg-function-polynomial.c \
	cpg-function-polynomial-piece.c \
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
	cpg-link-action.c \
	cpg-math.c \
	cpg-monitor.c \
	cpg-network.c \
	cpg-network-deserializer.c \
	cpg-network-serializer.c \
	cpg-object.c \
	cpg-property.c \
	cpg-stack.c \
	cpg-state.c \
	cpg-tokenizer.c

BUILT_SOURCES = \
	cpg-enum-types.c \
	cpg-enum-types.h \
	cpg-marshal.c \
	cpg-marshal.h

NOINST_H_FILES = \
	cpg-tokenizer.h \
	cpg-network-xml.h \
	cpg-stack-private.h \
	cpg-marshal.h

INST_H_FILES = \
	cpg-compile-context.h \
	cpg-compile-error.h \
	cpg-debug.h \
	cpg-expression.h \
	cpg-function.h \
	cpg-function-argument.h \
	cpg-function-polynomial.h \
	cpg-function-polynomial-piece.h \
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
	cpg-link-action.h \
	cpg-math.h \
	cpg-monitor.h \
	cpg-network.h \
	cpg-network-deserializer.h \
	cpg-network-serializer.h \
	cpg-object.h \
	cpg-property.h \
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

cpg-marshal.h: cpg-marshal.list $(GLIB_GENMARSHAL)
	$(AM_V_GEN) $(GLIB_GENMARSHAL) $< --header --prefix=cpg_marshal > $@

cpg-marshal.c: cpg-marshal.list $(GLIB_GENMARSHAL)
	$(AM_V_GEN) echo "#include \"cpg-marshal.h\"" > $@ && \
	$(GLIB_GENMARSHAL) $< --body --prefix=cpg_marshal >> $@

libcpg_network_2_0_la_CFLAGS = -D_GNU_SOURCE -Wall $(CPG_NETWORK_CFLAGS)
libcpg_network_2_0_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*" $(CPG_NETWORK_LIBS) $(INTROSPECTION_LIBS) -lm
libcpg_network_2_0_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network

libcpg_network_2_0_la_SOURCES = $(COMMON_SOURCES)

libcpg_network_2_0_include_HEADERS = $(libcpg_network_headers)

CLEANFILES = $(BUILT_SOURCES)

EXTRA_DIST = \
	$(NOINST_H_FILES) \
	cpg-enum-types.c.template \
	cpg-enum-types.h.template \
	cpg-marshal.list

if HAVE_INTROSPECTION
-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS = Cpg-2.0.gir

Cpg-2.0.gir: libcpg-network-2.0.la
INTROSPECTION_SCANNER_ARGS = -I$(top_srcdir) --warn-all --warn-error --verbose
Cpg_2_0_gir_NAMESPACE = Cpg
Cpg_2_0_gir_VERSION = 2.0
Cpg_2_0_gir_LIBS = libcpg-network-2.0.la
Cpg_2_0_gir_FILES = $(INST_H_FILES) $(COMMON_SOURCES)
Cpg_2_0_gir_INCLUDES = GLib-2.0 GObject-2.0 Gio-2.0

girdir = $(datadir)/gir-1.0
gir_DATA = $(INTROSPECTION_GIRS)

typelibdir = $(libdir)/girepository-1.0
typelib_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

CLEANFILES +=			\
	$(gir_DATA)	\
	$(typelib_DATA)
endif

-include $(top_srcdir)/git.mk
