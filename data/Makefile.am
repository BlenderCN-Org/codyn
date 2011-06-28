SUBDIRS = library

dtddir = $(pkgdatadir)-@CPG_NETWORK_API_VERSION@/dtd
dtd_DATA = cpg-network.dtd

mimedir = $(datadir)/mime/packages
mime_DATA = cpg.xml

lang3dir = $(datadir)/gtksourceview-3.0/language-specs
lang3_DATA = cpg.lang

lang2dir = $(datadir)/gtksourceview-2.0/language-specs
lang2_DATA = cpg.lang

latexdir = $(datadir)/texmf/tex/latex/cpg
latex_DATA = cpg.sty

emacsdir = $(datadir)/emacs/site-lisp
emacs_DATA = cpg-mode.el

EXTRA_DIST = \
	$(dtd_DATA) \
	cpg.xml.in \
	cpg.lang \
	cpg.sty \
	cpg-mode.el

CLEANFILES = cpg.xml

update_mime_database = update-mime-database $(datadir)/mime

update_tex = texhash

install-data-hook:
	if echo "Updating mime database" && $(update_mime_database); then       \
		echo "Done.";                           			\
	else                                    				\
		echo "*** Mime database not updated. After install, run this:";	\
		echo $(update_mime_database);                    		\
	fi;									\
	echo "Updating tex database" && $(update_tex) && echo "Done."

uninstall-hook:
	echo "Updating mime database" && $(update_mime_database) && echo "Done."; \
	echo "Updating tex database" && $(update_tex) && echo "Done."

-include $(top_srcdir)/git.mk
