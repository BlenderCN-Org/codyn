SUBDIRS = library

mimedir = $(datadir)/mime/packages
mime_DATA = cdn.xml

if ENABLE_GEDIT3
lang3dir = $(datadir)/gtksourceview-3.0/language-specs
lang3_DATA = cdn.lang
else
lang2dir = $(datadir)/gtksourceview-2.0/language-specs
lang2_DATA = cdn.lang
endif

latexdir = $(datadir)/texmf/tex/latex/cdn
latex_DATA = codyn.sty cdn-listings.tex

emacsdir = $(datadir)/emacs/site-lisp
emacs_DATA = codyn-mode.el

EXTRA_DIST = \
	$(dtd_DATA) \
	cdn.xml.in \
	cdn.lang \
	$(latex_DATA) \
	codyn-mode.el

CLEANFILES = cdn.xml

update_mime_database = update-mime-database $(datadir)/mime

update_tex = texhash

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
