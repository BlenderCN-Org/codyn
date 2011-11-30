INCLUDES =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_srcdir)		\
	$(CODYN_CFLAGS)

bin_PROGRAMS = cdn-render

cdn_render_SOURCES = \
	cdn-render.c

EXTRA_DIST = $(NOINST_H_FILES)

cdn_render_LDADD = $(top_builddir)/codyn/libcodyn-$(CODYN_API_VERSION).la -lm $(CODYN_LIBS)

-include $(top_srcdir)/git.mk
