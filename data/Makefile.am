SUBDIRS = library

mimedir = $(datadir)/mime/packages
mime_DATA = cdn.xml

langdir = $(datadir)/gtksourceview-3.0/language-specs
lang_DATA = cdn.lang

latexdir = $(datadir)/texmf/tex/latex/cdn
latex_DATA = codyn.sty cdn-listings.tex

emacsdir = $(datadir)/emacs/site-lisp
emacs_DATA = codyn-mode.el

EXTRA_DIST =		\
	$(dtd_DATA)	\
	cdn.xml.in	\
	$(latex_DATA) 	\
	codyn-mode.el	\
	keywords.list	\
	selectors.list	\
	cdn.lang	\
	cdn.lang.in	\
	make-cdn-lang.py

CLEANFILES = cdn.xml

update_mime_database = update-mime-database $(datadir)/mime

update_tex = texhash

keywords.list: $(top_srcdir)/codyn/cdn-parser-tokens.l
	$(MAKE) -C $(top_builddir)/codyn extract-keywords > $@

selectors.list: $(top_srcdir)/codyn/cdn-parser-tokens.l
	$(MAKE) -C $(top_builddir)/codyn extract-selectors > $@

cdn.lang: cdn.lang.in keywords.list selectors.list
	$(AM_V_GEN) $(PYTHON) $(srcdir)/make-cdn-lang.py < $(srcdir)/cdn.lang.in > $@

cdn-listings.tex: cdn-listings.tex.in keywords.list selectors.list
	$(AM_V_GEN) $(PYTHON) $(srcdir)/make-cdn-listings.py < $(srcdir)/cdn-listings.tex.in > $@


install-data-local-hook:
	if echo "Updating mime database" && $(update_mime_database); then       \
		echo "Done.";                           			\
	else                                    				\
		echo "*** Mime database not updated. After install, run this:";	\
		echo $(update_mime_database);                    		\
	fi;									\
	echo "Updating tex database" && $(update_tex) && echo "Done."

uninstall-local-hook:
	echo "Updating mime database" && $(update_mime_database) && echo "Done."; \
	echo "Updating tex database" && $(update_tex) && echo "Done."

-include $(top_srcdir)/git.mk
