AM_CPPFLAGS =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"

noinst_LTLIBRARIES = libinstructions.la

libinstructions_la_SOURCES = 			\
	cdn-instruction.c			\
	cdn-instruction-boxed.c			\
	cdn-instruction-custom-function.c	\
	cdn-instruction-custom-function-ref.c	\
	cdn-instruction-custom-operator.c	\
	cdn-instruction-custom-operator-ref.c	\
	cdn-instruction-function.c		\
	cdn-instruction-index.c			\
	cdn-instruction-number.c		\
	cdn-instruction-matrix.c		\
	cdn-instruction-variable.c		\
	cdn-instruction-rand.c

libinstructions_headers =			\
	cdn-instruction.h			\
	cdn-instruction-boxed.h			\
	cdn-instructions.h			\
	cdn-instruction-custom-function.h	\
	cdn-instruction-custom-function-ref.h	\
	cdn-instruction-custom-operator.h	\
	cdn-instruction-custom-operator-ref.h	\
	cdn-instruction-function.h		\
	cdn-instruction-index.h			\
	cdn-instruction-number.h		\
	cdn-instruction-matrix.h		\
	cdn-instruction-variable.h		\
	cdn-instruction-rand.h

libinstructions_includedir = $(includedir)/codyn-$(CODYN_API_VERSION)/codyn/instructions
libinstructions_include_HEADERS = $(libinstructions_headers)

libinstructions_la_CFLAGS = -D_GNU_SOURCE -Wall $(CODYN_CFLAGS)

-include $(top_srcdir)/git.mk
