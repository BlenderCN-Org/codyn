AM_CPPFLAGS =				\
	-I$(srcdir)			\
	-I$(builddir)			\
	-I$(top_builddir)/tests		\
	-I$(top_srcdir)/tests		\
	-I$(top_srcdir)			\
	$(CODYN_CFLAGS)

TEST_PROGS = expression network direct import template object node variable operator integrator interface

#TEST_PROGS += file

progs_ldadd = $(top_builddir)/codyn/libcodyn-$(CODYN_API_VERSION).la $(CODYN_LIBS)

noinst_PROGRAMS = $(TEST_PROGS)

expression_SOURCES = expression.c utils.h utils.c
expression_LDADD = $(progs_ldadd)

network_SOURCES = network.c utils.h utils.c
network_LDADD = $(progs_ldadd)

direct_SOURCES = direct.c utils.h utils.c
direct_LDADD = $(progs_ldadd)

import_SOURCES = import.c utils.h utils.c
import_LDADD = $(progs_ldadd)

template_SOURCES = template.c utils.h utils.c
template_LDADD = $(progs_ldadd)

object_SOURCES = object.c utils.h utils.c
object_LDADD = $(progs_ldadd)

node_SOURCES = node.c utils.h utils.c
node_LDADD = $(progs_ldadd)

variable_SOURCES = variable.c utils.h utils.c
variable_LDADD = $(progs_ldadd)

operator_SOURCES = operator.c utils.h utils.c
operator_LDADD = $(progs_ldadd)

integrator_SOURCES = integrator.c utils.h utils.c
integrator_LDADD = $(progs_ldadd)

interface_SOURCES = interface.c utils.h utils.c
interface_LDADD = $(progs_ldadd)

#file_SOURCES = file.c utils.h utils.c
#file_LDADD = $(progs_ldadd)

TESTS = $(TEST_PROGS)

EXTRA_DIST = \
	test_import.cdn \
	test_import_templates.cdn \
	import.cdn \
	test_node_integrate_multiple_euler.cdn \
	test_interface_templates.cdn \
	test_input.txt \
	test_file.cdn

-include $(top_srcdir)/git.mk
