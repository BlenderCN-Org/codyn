INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	-I$(top_srcdir)/cpg-network	\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"

noinst_LTLIBRARIES = liboperators.la

liboperators_la_SOURCES = 	\
	cpg-operator.c \
	cpg-operator-delayed.c \
	cpg-operator-diff.c \
	cpg-operator-pdiff.c \
	cpg-operator-simplify.c \
	cpg-operator-df-dt.c \
	cpg-operator-linsolve.c

liboperators_headers =			\
	cpg-operator.h \
	cpg-operator-delayed.h \
	cpg-operator-diff.h \
	cpg-operator-pdiff.h \
	cpg-operator-simplify.h \
	cpg-operator-df-dt.h \
	cpg-operator-linsolve.h

liboperators_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network/operators
liboperators_include_HEADERS = $(liboperators_headers)

liboperators_la_CFLAGS = -D_GNU_SOURCE -Wall $(CPG_NETWORK_CFLAGS)

-include $(top_srcdir)/git.mk
