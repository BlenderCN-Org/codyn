AM_CPPFLAGS =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	-I$(top_srcdir)/codyn		\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"

noinst_LTLIBRARIES = liboperators.la

liboperators_la_SOURCES = 	\
	cdn-operator.c \
	cdn-operator-delayed.c \
	cdn-operator-diff.c \
	cdn-operator-pdiff.c \
	cdn-operator-simplify.c \
	cdn-operator-dt.c \
	cdn-operator-linsolve.c

liboperators_headers =			\
	cdn-operator.h \
	cdn-operator-delayed.h \
	cdn-operator-diff.h \
	cdn-operator-pdiff.h \
	cdn-operator-simplify.h \
	cdn-operator-dt.h \
	cdn-operator-linsolve.h

liboperators_includedir = $(includedir)/codyn-$(CODYN_API_VERSION)/codyn/operators
liboperators_include_HEADERS = $(liboperators_headers)

liboperators_la_CFLAGS = -D_GNU_SOURCE -Wall $(CODYN_CFLAGS)

-include $(top_srcdir)/git.mk
