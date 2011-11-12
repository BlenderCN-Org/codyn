INCLUDES =				\
	-I$(srcdir)			\
	-I$(top_srcdir)			\
	$(INTROSPECTION_CFLAGS)		\
	-DDATADIR=\""$(datadir)"\"

noinst_LTLIBRARIES = libinstructions.la

libinstructions_la_SOURCES = 			\
	cpg-instruction.c			\
	cpg-instruction-custom-function.c	\
	cpg-instruction-custom-function-ref.c	\
	cpg-instruction-custom-operator.c	\
	cpg-instruction-custom-operator-ref.c	\
	cpg-instruction-function.c		\
	cpg-instruction-number.c		\
	cpg-instruction-operator.c		\
	cpg-instruction-property.c		\
	cpg-instruction-variadic-function.c	\
	cpg-instruction-constant.c

libinstructions_headers =			\
	cpg-instruction.h			\
	cpg-instructions.h			\
	cpg-instruction-custom-function.h	\
	cpg-instruction-custom-function-ref.h	\
	cpg-instruction-custom-operator.h	\
	cpg-instruction-custom-operator-ref.h	\
	cpg-instruction-function.h		\
	cpg-instruction-number.h		\
	cpg-instruction-operator.h		\
	cpg-instruction-property.h		\
	cpg-instruction-variadic-function.h	\
	cpg-instruction-constant.h

libinstructions_includedir = $(includedir)/cpg-network-$(CPG_NETWORK_API_VERSION)/cpg-network/instructions
libinstructions_include_HEADERS = $(libinstructions_headers)

libinstructions_la_CFLAGS = -D_GNU_SOURCE -Wall $(CPG_NETWORK_CFLAGS)

-include $(top_srcdir)/git.mk
