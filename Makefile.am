SUBDIRS = src tests

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = \
	cpg-network-@CPG_NETWORK_API_VERSION@.pc

if ENABLE_RT
pkgconfig_DATA += cpg-network-rt-@CPG_NETWORK_API_VERSION@.pc
endif

cpg-network-@CPG_NETWORK_API_VERSION@.pc: cpg-network.pc
	cp cpg-network.pc cpg-network-@CPG_NETWORK_API_VERSION@.pc

if ENABLE_RT
cpg-network-rt-@CPG_NETWORK_API_VERSION@.pc: cpg-network-rt.pc
	cp cpg-network-rt.pc cpg-network-rt-@CPG_NETWORK_API_VERSION@.pc
endif

EXTRA_DIST = cpg-network.pc.in

if ENABLE_RT
EXTRA_DIST += cpg-network-rt.pc.in
endif

DISTCHECK_CONFIGURE_FLAGS = --disable-threads --enable-rt

CLEANFILES = 	\
	$(pkgconfig_DATA)
