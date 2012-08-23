SUBDIRS = instructions operators integrators tree-algorithms

INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"	\
	-DLIBDIR=\""$(libdir)"\"

AM_YFLAGS = -d

COMMON_SOURCES = 		\
	cdn-annotatable.c	\
	cdn-debug.c \
	cdn-compile-context.c \
	cdn-compile-error.c \
	cdn-phaseable.c \
	cdn-enum-types.c \
	cdn-marshal.c \
	cdn-expression.c \
	cdn-expression-tree-iter.c \
	cdn-function.c \
	cdn-function-argument.c \
	cdn-function-polynomial.c \
	cdn-function-polynomial-piece.c \
	cdn-node.c \
	cdn-io.c \
	cdn-integrators.c \
	cdn-import.c \
	cdn-import-alias.c \
	cdn-edge.c \
	cdn-edge-action.c \
	cdn-math.c \
	cdn-mini-object.c \
	cdn-modifiable.c \
	cdn-monitor.c \
	cdn-network.c \
	cdn-network-deserializer.c \
	cdn-network-serializer.c \
	cdn-network-xml.c \
	cdn-network-parser-utils.c \
	cdn-parser-context.c \
	cdn-embedded-string.c \
	cdn-expansion-context.c \
	cdn-object.c \
	cdn-operators.c \
	cdn-variable.c \
	cdn-variable-interface.c \
	cdn-selector.c \
	cdn-network-simplify.c \
	cdn-stack.c \
	cdn-tokenizer.c \
	cdn-usable.c \
	cdn-utils.c \
	cdn-parser.y \
	cdn-parser-tokens.l \
	cdn-layout.c \
	cdn-expansion.c \
	cdn-selection.c \
	cdn-attribute.c \
	cdn-layoutable.c \
	cdn-statement.c \
	cdn-taggable.c \
	cdn-event.c \
	cdn-io-method.c \
	cdn-cfile-stream.c

BUILT_SOURCES = \
	cdn-enum-types.c \
	cdn-enum-types.h \
	cdn-marshal.c \
	cdn-marshal.h \
	cdn-parser.h \
	cdn-parser-tokens.c

NOINST_H_FILES = \
	cdn-tokenizer.h \
	cdn-network-xml.h \
	cdn-stack-private.h \
	cdn-network-parser-utils.h \
	cdn-marshal.h

INST_H_FILES = \
	codyn.h \
	cdn-annotatable.h \
	cdn-debug.h \
	cdn-compile-context.h \
	cdn-phaseable.h \
	cdn-compile-error.h \
	cdn-expression.h \
	cdn-expression-tree-iter.h \
	cdn-function.h \
	cdn-function-argument.h \
	cdn-function-polynomial.h \
	cdn-function-polynomial-piece.h \
	cdn-node.h \
	cdn-io.h \
	cdn-integrators.h \
	cdn-import.h \
	cdn-import-alias.h \
	cdn-edge.h \
	cdn-edge-action.h \
	cdn-math.h \
	cdn-mini-object.h \
	cdn-modifiable.h \
	cdn-monitor.h \
	cdn-network.h \
	cdn-network-deserializer.h \
	cdn-network-serializer.h \
	cdn-object.h \
	cdn-operators.h \
	cdn-parser-context.h \
	cdn-variable.h \
	cdn-variable-interface.h \
	cdn-selector.h \
	cdn-stack.h \
	cdn-usable.h \
	cdn-utils.h \
	cdn-layout.h \
	cdn-embedded-string.h \
	cdn-expansion-context.h \
	cdn-expansion.h \
	cdn-selection.h \
	cdn-attribute.h \
	cdn-layoutable.h \
	cdn-statement.h \
	cdn-taggable.h \
	cdn-event.h \
	cdn-forward-decl.h \
	cdn-io-method.h \
	cdn-cfile-stream.h

lib_LTLIBRARIES = libcodyn-3.0.la

libcodyn_headers = \
	$(INST_H_FILES) \
	cdn-enum-types.h

cdn-enum-types.h: cdn-enum-types.h.template $(INST_H_FILES) $(GLIB_MKENUMS)
	(cd $(srcdir) && $(GLIB_MKENUMS) --template cdn-enum-types.h.template $(INST_H_FILES)) > $@

cdn-enum-types.c: cdn-enum-types.c.template cdn-enum-types.h $(INST_H_FILES) $(GLIB_MKENUMS)
	(cd $(srcdir) && $(GLIB_MKENUMS) --template cdn-enum-types.c.template $(INST_H_FILES)) > $@

cdn-marshal.h: cdn-marshal.list $(GLIB_GENMARSHAL)
	$(AM_V_GEN) $(GLIB_GENMARSHAL) $< --header --prefix=cdn_marshal > $@

cdn-marshal.c: cdn-marshal.list $(GLIB_GENMARSHAL)
	$(AM_V_GEN) echo "#include \"cdn-marshal.h\"" > $@ && \
	$(GLIB_GENMARSHAL) $< --body --prefix=cdn_marshal >> $@

libcodyn_3_0_la_CFLAGS = -D_GNU_SOURCE -Wall $(CODYN_CFLAGS) $(LAPACK_CFLAGS) $(BLAS_CFLAGS)
libcodyn_3_0_la_LIBADD = instructions/libinstructions.la operators/liboperators.la integrators/libintegrators.la tree-algorithms/libtreealgorithms.la

libcodyn_3_0_la_LDFLAGS = -export-dynamic -no-undefined -export-symbols-regex "[^_].*" $(CODYN_LIBS) $(INTROSPECTION_LIBS) $(LAPACK_LIBS) $(BLAS_LIBS) -lm
libcodyn_3_0_includedir = $(includedir)/codyn-$(CODYN_API_VERSION)/codyn

libcodyn_3_0_la_SOURCES = $(COMMON_SOURCES)

libcodyn_3_0_include_HEADERS = $(libcodyn_headers)

CLEANFILES = $(BUILT_SOURCES) cdn-parser.c

EXTRA_DIST = \
	$(NOINST_H_FILES) \
	cdn-enum-types.c.template \
	cdn-enum-types.h.template \
	cdn-marshal.list

if ENABLE_PYTHON
overridesdir = $(pyoverridesdir)
overrides_PYTHON =                      \
        Cdn.py
endif

EXTRA_DIST += Cdn.py

if HAVE_INTROSPECTION
-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS = Cdn-3.0.gir

INSTRUCTIONHEADERS = \
	instructions/cdn-instruction-custom-function.h \
	instructions/cdn-instruction-custom-operator.h \
	instructions/cdn-instruction-function.h \
	instructions/cdn-instruction.h \
	instructions/cdn-instruction-boxed.h \
	instructions/cdn-instruction-number.h \
	instructions/cdn-instruction-matrix.h \
	instructions/cdn-instruction-variable.h \
	instructions/cdn-instructions.h \
	instructions/cdn-instruction-rand.h

INSTRUCTIONSOURCES = \
	instructions/cdn-instruction.c \
	instructions/cdn-instruction-boxed.c \
	instructions/cdn-instruction-custom-function.c \
	instructions/cdn-instruction-custom-operator.c \
	instructions/cdn-instruction-custom-function-ref.c \
	instructions/cdn-instruction-custom-operator-ref.c \
	instructions/cdn-instruction-function.c \
	instructions/cdn-instruction-number.c \
	instructions/cdn-instruction-matrix.c \
	instructions/cdn-instruction-variable.c \
	instructions/cdn-instruction-rand.c

OPERATORSOURCES = \
	operators/cdn-operator.c \
	operators/cdn-operator-delayed.c \
	operators/cdn-operator-diff.c \
	operators/cdn-operator-pdiff.c \
	operators/cdn-operator-simplify.c \
	operators/cdn-operator-dt.c \
	operators/cdn-operator-linsolve.c

OPERATORHEADERS = \
	operators/cdn-operator.h \
	operators/cdn-operator-delayed.h \
	operators/cdn-operator-diff.h \
	operators/cdn-operator-pdiff.h \
	operators/cdn-operator-simplify.h \
	operators/cdn-operator-dt.h \
	operators/cdn-operator-linsolve.h

INTEGRATORSOURCES = 	\
	integrators/cdn-integrator.c \
	integrators/cdn-integrator-state.c \
	integrators/cdn-integrator-euler.c \
	integrators/cdn-integrator-leap-frog.c \
	integrators/cdn-integrator-runge-kutta.c \
	integrators/cdn-integrator-predict-correct.c

INTEGRATORHEADERS =			\
	integrators/cdn-integrator.h \
	integrators/cdn-integrator-state.h \
	integrators/cdn-integrator-euler.h \
	integrators/cdn-integrator-leap-frog.h \
	integrators/cdn-integrator-runge-kutta.h \
	integrators/cdn-integrator-predict-correct.h

TREEALGORITHMSSOURCES = 	\
	tree-algorithms/cdn-tree-algorithms-canonicalize.c \
	tree-algorithms/cdn-tree-algorithms-linsolve.c \
	tree-algorithms/cdn-tree-algorithms-simplify.c \
	tree-algorithms/cdn-tree-algorithms-derive.c \
	tree-algorithms/cdn-tree-algorithms-string.c \
	tree-algorithms/cdn-tree-algorithms-substitute.c

Cdn-3.0.gir: libcodyn-3.0.la

INTROSPECTION_SCANNER_ARGS = 			\
	-I$(top_srcdir) 			\
	--warn-all 				\
	--verbose 				\
	--c-include=codyn/codyn.h

Cdn_3_0_gir_NAMESPACE = Cdn
Cdn_3_0_gir_VERSION = 3.0

Cdn_3_0_gir_LIBS = 				\
	libcodyn-3.0.la 			\
	instructions/libinstructions.la 	\
	operators/liboperators.la 		\
	integrators/libintegrators.la 		\
	tree-algorithms/libtreealgorithms.la

Cdn_3_0_gir_FILES = 				\
	$(INST_H_FILES) 			\
	$(COMMON_SOURCES) 			\
	$(INSTRUCTIONHEADERS) 			\
	$(INSTRUCTIONSOURCES) 			\
	$(OPERATORHEADERS) 			\
	$(OPERATORSOURCES) 			\
	$(INTEGRATORHEADERS) 			\
	$(INTEGRATORSOURCES) 			\
	$(TREEALGORITHMSSOURCES) 		\
	cdn-enum-types.h cdn-enum-types.c

Cdn_3_0_gir_INCLUDES = GLib-2.0 GObject-2.0 Gio-2.0

girdir = $(datadir)/gir-1.0
gir_DATA = $(INTROSPECTION_GIRS)

typelibdir = $(libdir)/girepository-1.0
typelib_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

CLEANFILES +=			\
	$(gir_DATA)	\
	$(typelib_DATA)
endif

-include $(top_srcdir)/git.mk
