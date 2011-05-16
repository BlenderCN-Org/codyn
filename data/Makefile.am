dtddir = $(pkgdatadir)-@CPG_NETWORK_API_VERSION@/dtd
dtd_DATA = cpg-network.dtd

librarydir = $(pkgdatadir)-@CPG_NETWORK_API_VERSION@/import-library
library_DATA = \
	hopf.cpg \
	phase.cpg

mimedir = $(datadir)/mime/packages
mime_DATA = cpg.xml

lang3dir = $(datadir)/gtksourceview-3.0/language-specs
lang3_DATA = cpg.lang

lang2dir = $(datadir)/gtksourceview-2.0/language-specs
lang2_DATA = cpg.lang

EXTRA_DIST = \
	$(dtd_DATA) \
	$(library_DATA) \
	cpg.xml.in \
	cpg.lang

CLEANFILES = cpg.xml

update_mime_database = update-mime-database $(datadir)/mime

install-data-hook:
	if echo "Updating mime database" && $(update_mime_database); then        \
		echo "Done.";                            \
	else                                    \
		echo "*** Mime database not updated. After install, run this:";\
		echo $(update_mime_database);                    \
	fi
uninstall-hook:
	echo "Updating mime database" && $(update_mime_database) && echo "Done."

-include $(top_srcdir)/git.mk
