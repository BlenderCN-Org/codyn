AM_CPPFLAGS =				\
	-I$(srcdir)			\
	-I$(builddir)			\
	-I$(top_srcdir)			\
	$(CODYN_CFLAGS)			\
	-DDATADIR="\"$(datadir)\""	\
	-DLIBDIR="\"$(libdir)\""

if ENABLE_OSX_FRAMEWORK
fwprefix=$(dir $(prefix))

AM_CPPFLAGS += -DENABLE_OSX_FRAMEWORK=1 -DPREFIX="\"$(fwprefix)\""
endif

bin_PROGRAMS = cdn-python

cdn_python_SOURCES = \
	cdn-python.c

-include $(top_srcdir)/git.mk
