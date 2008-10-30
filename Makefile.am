SUBDIRS = src tests

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = cpg-network-@CPG_NETWORK_API_VERSION@.pc

cpg-network-@CPG_NETWORK_API_VERSION@.pc: cpg-network.pc
	cp cpg-network.pc cpg-network-@CPG_NETWORK_API_VERSION@.pc

EXTR_DIST = 	\
	cpg-network.pc.in

CLEANFILES = 	\
	$(pkgconfig_DATA)
