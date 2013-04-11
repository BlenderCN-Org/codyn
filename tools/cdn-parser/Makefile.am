AM_CPPFLAGS =			\
	-I$(srcdir)		\
	-I$(builddir)		\
	-I$(top_builddir)/tests	\
	-I$(top_srcdir)/tests	\
	-I$(top_srcdir)		\
	$(CODYN_CFLAGS)

bin_PROGRAMS = cdn-parser

cdn_parser_SOURCES = \
	cdn-parser.c

cdn_parser_LDADD = $(top_builddir)/codyn/libcodyn-$(CODYN_API_VERSION).la -lm $(CODYN_LIBS) $(CDN_PARSER_LIBS)

-include $(top_srcdir)/git.mk
