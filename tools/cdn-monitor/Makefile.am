INCLUDES =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_builddir)/tests	\
	-I$(top_srcdir)/tests	\
	-I$(top_srcdir)		\
	$(CODYN_CFLAGS)

bin_PROGRAMS = cdn-monitor

cdn_monitor_SOURCES = \
	cdn-monitor.c

cdn_monitor_LDADD = $(top_builddir)/codyn/libcodyn-$(CODYN_API_VERSION).la -lm $(CODYN_LIBS)

-include $(top_srcdir)/git.mk
