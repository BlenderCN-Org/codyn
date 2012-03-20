INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	-I$(top_srcdir)/codyn	\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"

noinst_LTLIBRARIES = libintegrators.la

libintegrators_la_SOURCES = 	\
	cdn-integrator.c \
	cdn-integrator-state.c \
	cdn-integrator-euler.c \
	cdn-integrator-leap-frog.c \
	cdn-integrator-runge-kutta.c \
	cdn-integrator-predict-correct.c

libintegrators_headers =			\
	cdn-integrator.h \
	cdn-integrator-state.h \
	cdn-integrator-euler.h \
	cdn-integrator-leap-frog.h \
	cdn-integrator-runge-kutta.h \
	cdn-integrator-predict-correct.h

libintegrators_includedir = $(includedir)/codyn-$(CODYN_API_VERSION)/codyn/integrators
libintegrators_include_HEADERS = $(libintegrators_headers)

libintegrators_la_CFLAGS = -D_GNU_SOURCE -Wall $(CODYN_CFLAGS)

-include $(top_srcdir)/git.mk
