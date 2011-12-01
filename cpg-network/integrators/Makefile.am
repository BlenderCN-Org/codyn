INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	-I$(top_srcdir)/cpg-network	\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"

noinst_LTLIBRARIES = libintegrators.la

libintegrators_la_SOURCES = 	\
	cpg-integrator.c \
	cpg-integrator-state.c \
	cpg-integrator-euler.c \
	cpg-integrator-leap-frog.c \
	cpg-integrator-runge-kutta.c \
	cpg-integrator-predict-correct.c \
	cpg-integrator-stub.c

libintegrators_headers =			\
	cpg-integrator.h \
	cpg-integrator-state.h \
	cpg-integrator-euler.h \
	cpg-integrator-leap-frog.h \
	cpg-integrator-runge-kutta.h \
	cpg-integrator-predict-correct.h \
	cpg-integrator-stub.h

libintegrators_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network/integrators
libintegrators_include_HEADERS = $(libintegrators_headers)

libintegrators_la_CFLAGS = -D_GNU_SOURCE -Wall $(CPG_NETWORK_CFLAGS)

-include $(top_srcdir)/git.mk
