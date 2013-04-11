AM_CPPFLAGS =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_builddir)/tests	\
	-I$(top_srcdir)/tests	\
	-I$(top_srcdir)		\
	$(CODYN_CFLAGS)

bin_PROGRAMS = cdn-context

cdn_context_SOURCES = \
	cdn-context.c

cdn_context_LDADD = $(top_builddir)/codyn/libcodyn-$(CODYN_API_VERSION).la -lm $(CODYN_LIBS) $(CDN_CONTEXT_LIBS)

-include $(top_srcdir)/git.mk
