common_cppflags =			\
	-I$(srcdir)			\
	-I$(builddir)			\
	-I$(top_srcdir)			\
	$(CODYN_CFLAGS)			\
	-DDATADIR="\"$(datadir)\""	\
	-DLIBDIR="\"$(libdir)\""

if ENABLE_OSX_FRAMEWORK
fwprefix=$(dir $(prefix))

common_cppflags += -DENABLE_OSX_FRAMEWORK=1 -DPREFIX="\"$(fwprefix)\""
endif

bin_PROGRAMS = cdn-python cdn-python3

cdn_python_SOURCES = \
	cdn-python.c

cdn_python_CPPFLAGS = $(common_cppflags) -DPYTHON="\"python\""

cdn_python3_SOURCES = \
	cdn-python.c

cdn_python3_CPPFLAGS = $(common_cppflags) -DPYTHON="\"python3\""

-include $(top_srcdir)/git.mk
