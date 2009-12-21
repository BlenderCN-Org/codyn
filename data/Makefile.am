dtddir = $(pkgdatadir)-@CPG_NETWORK_API_VERSION@/dtd
dtd_DATA = cpg-network.dtd

librarydir = $(pkgdatadir)-@CPG_NETWORK_API_VERSION@/library
library_DATA = \
	hopf.cpg \
	phase.cpg

EXTRA_DIST = \
	$(dtd_DATA) \
	$(library_DATA)

-include $(top_srcdir)/git.mk
