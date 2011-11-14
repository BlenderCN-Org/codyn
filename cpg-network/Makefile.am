SUBDIRS = instructions

INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"

AM_YFLAGS = -d

COMMON_SOURCES = 		\
	cpg-annotatable.c	\
	cpg-compile-context.c \
	cpg-compile-error.c \
	cpg-enum-types.c \
	cpg-marshal.c \
	cpg-expression.c \
	cpg-expression-tree-iter.c \
	cpg-function.c \
	cpg-function-argument.c \
	cpg-function-polynomial.c \
	cpg-function-polynomial-piece.c \
	cpg-group.c \
	cpg-input.c \
	cpg-input-file.c \
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
	cpg-mini-object.c \
	cpg-modifiable.c \
	cpg-monitor.c \
	cpg-network.c \
	cpg-network-deserializer.c \
	cpg-network-serializer.c \
	cpg-network-parser-utils.c \
	cpg-parser-context.c \
	cpg-embedded-string.c \
	cpg-embedded-context.c \
	cpg-object.c \
	cpg-operator.c \
	cpg-operators.c \
	cpg-operator-delayed.c \
	cpg-operator-diff.c \
	cpg-operator-pdiff.c \
	cpg-operator-simplify.c \
	cpg-operator-df-dt.c \
	cpg-property.c \
	cpg-property-interface.c \
	cpg-selector.c \
	cpg-stack.c \
	cpg-symbolic.c \
	cpg-tokenizer.c \
	cpg-usable.c \
	cpg-utils.c \
	cpg-parser.y \
	cpg-parser-tokens.l \
	cpg-layout.c \
	cpg-expansion.c \
	cpg-selection.c \
	cpg-attribute.c \
	cpg-layoutable.c \
	cpg-statement.c \
	cpg-taggable.c \
	cpg-parser-code.c

BUILT_SOURCES = \
	cpg-enum-types.c \
	cpg-enum-types.h \
	cpg-marshal.c \
	cpg-marshal.h \
	cpg-parser.h \
	cpg-parser-tokens.c

NOINST_H_FILES = \
	cpg-tokenizer.h \
	cpg-network-xml.h \
	cpg-stack-private.h \
	cpg-network-parser-utils.h \
	cpg-marshal.h

INST_H_FILES = \
	cpg-annotatable.h \
	cpg-compile-context.h \
	cpg-compile-error.h \
	cpg-expression.h \
	cpg-expression-tree-iter.h \
	cpg-function.h \
	cpg-function-argument.h \
	cpg-function-polynomial.h \
	cpg-function-polynomial-piece.h \
	cpg-group.h \
	cpg-input.h \
	cpg-input-file.h \
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
	cpg-mini-object.h \
	cpg-modifiable.h \
	cpg-monitor.h \
	cpg-network.h \
	cpg-network-deserializer.h \
	cpg-network-serializer.h \
	cpg-object.h \
	cpg-operator.h \
	cpg-operators.h \
	cpg-operator-delayed.h \
	cpg-operator-diff.h \
	cpg-operator-pdiff.h \
	cpg-operator-simplify.h \
	cpg-operator-df-dt.h \
	cpg-parser-context.h \
	cpg-property.h \
	cpg-property-interface.h \
	cpg-selector.h \
	cpg-stack.h \
	cpg-symbolic.h \
	cpg-usable.h \
	cpg-utils.h \
	cpg-layout.h \
	cpg-embedded-string.h \
	cpg-embedded-context.h \
	cpg-expansion.h \
	cpg-selection.h \
	cpg-attribute.h \
	cpg-layoutable.h \
	cpg-statement.h \
	cpg-taggable.h \
	cpg-parser-code.h

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
libcpg_network_2_0_la_LIBADD = instructions/libinstructions.la

libcpg_network_2_0_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*" $(CPG_NETWORK_LIBS) $(INTROSPECTION_LIBS) -lm
libcpg_network_2_0_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network

libcpg_network_2_0_la_SOURCES = $(COMMON_SOURCES)

libcpg_network_2_0_include_HEADERS = $(libcpg_network_headers)

CLEANFILES = $(BUILT_SOURCES) cpg-parser.c

EXTRA_DIST = \
	$(NOINST_H_FILES) \
	cpg-enum-types.c.template \
	cpg-enum-types.h.template \
	cpg-marshal.list

if HAVE_INTROSPECTION
-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS = Cpg-2.0.gir

INSTRUCTIONHEADERS = \
	instructions/cpg-instruction-custom-function.h \
	instructions/cpg-instruction-custom-operator.h \
	instructions/cpg-instruction-function.h \
	instructions/cpg-instruction.h \
	instructions/cpg-instruction-number.h \
	instructions/cpg-instruction-operator.h \
	instructions/cpg-instruction-property.h \
	instructions/cpg-instructions.h \
	instructions/cpg-instruction-variadic-function.h

INSTRUCTIONSOURCES = \
	instructions/cpg-instruction.c \
	instructions/cpg-instruction-custom-function.c \
	instructions/cpg-instruction-custom-operator.c \
	instructions/cpg-instruction-custom-function-ref.c \
	instructions/cpg-instruction-custom-operator-ref.c \
	instructions/cpg-instruction-function.c \
	instructions/cpg-instruction-number.c \
	instructions/cpg-instruction-operator.c \
	instructions/cpg-instruction-property.c \
	instructions/cpg-instruction-variadic-function.c

Cpg-2.0.gir: libcpg-network-2.0.la
INTROSPECTION_SCANNER_ARGS = -I$(top_srcdir) --warn-all --verbose
Cpg_2_0_gir_NAMESPACE = Cpg
Cpg_2_0_gir_VERSION = 2.0
Cpg_2_0_gir_LIBS = libcpg-network-2.0.la instructions/libinstructions.la
Cpg_2_0_gir_FILES = $(INST_H_FILES) $(COMMON_SOURCES) $(INSTRUCTIONHEADERS) $(INSTRUCTIONSOURCES)
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
