INCLUDES =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_srcdir)		\
	$(CODYN_CFLAGS)

bin_PROGRAMS = cdn-compile

cdn_compile_SOURCES = \
	cdn-compile.c

cdn_compile_LDADD = $(top_builddir)/codyn/libcodyn-$(CODYN_API_VERSION).la -lm $(CODYN_LIBS) $(CDN_PARSER_LIBS)

-include $(top_srcdir)/git.mk
