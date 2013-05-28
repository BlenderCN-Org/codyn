AM_CPPFLAGS =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_builddir)/tests	\
	-I$(top_srcdir)/tests	\
	-I$(top_srcdir)		\
	$(CODYN_CFLAGS)

bin_PROGRAMS = cdn-monitor
bin_SCRIPTS = cdn-monitor-i

NOINST_H_FILES = \
	implementation.h \
	cdn-rawc-types.h \
	defines.h \
	monitor.h

cdn_monitor_SOURCES = \
	cdn-monitor.c \
	monitor.c \
	implementation.c \
	implementation-codyn.c \
	implementation-rawc.c \
	$(NOINST_H_FILES)

cdn_monitor_LDADD = $(top_builddir)/codyn/libcodyn-$(CODYN_API_VERSION).la -lm $(CODYN_LIBS)

-include $(top_srcdir)/git.mk
