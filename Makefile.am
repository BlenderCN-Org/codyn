SUBDIRS = cpg-network data tests docs

if ENABLE_WEBOTS
SUBDIRS += cpg-network-webots
endif

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = \
	cpg-network-@CPG_NETWORK_API_VERSION@.pc

if ENABLE_WEBOTS
pkgconfig_DATA += cpg-network-webots-@CPG_NETWORK_API_VERSION@.pc
endif

cpg-network-@CPG_NETWORK_API_VERSION@.pc: cpg-network.pc
	cp cpg-network.pc cpg-network-@CPG_NETWORK_API_VERSION@.pc

if ENABLE_WEBOTS
cpg-network-webots-@CPG_NETWORK_API_VERSION@.pc: cpg-network-webots.pc
	cp cpg-network-webots.pc cpg-network-webots-@CPG_NETWORK_API_VERSION@.pc
endif

EXTRA_DIST = cpg-network.pc.in

if ENABLE_WEBOTS
EXTRA_DIST += cpg-network-webots.pc.in
endif

DISTCHECK_CONFIGURE_FLAGS = --disable-threads --enable-gtk-doc --enable-webots

CLEANFILES = 	\
	$(pkgconfig_DATA)
