INCLUDES =				\
	-I$(srcdir)			\
	-I$(builddir)			\
	-I$(top_builddir)/tests		\
	-I$(top_srcdir)/tests		\
	-I$(top_srcdir)				\
	$(CPG_NETWORK_CFLAGS)

TEST_PROGS = expression network direct import template object group property operator integrator
progs_ldadd = $(top_builddir)/cpg-network/libcpg-network-$(CPG_NETWORK_API_VERSION).la

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

group_SOURCES = group.c utils.h utils.c
group_LDADD = $(progs_ldadd)

property_SOURCES = property.c utils.h utils.c
property_LDADD = $(progs_ldadd)

operator_SOURCES = operator.c utils.h utils.c
operator_LDADD = $(progs_ldadd)

integrator_SOURCES = integrator.c utils.h utils.c
integrator_LDADD = $(progs_ldadd)

TESTS = $(TEST_PROGS)

-include $(top_srcdir)/git.mk
