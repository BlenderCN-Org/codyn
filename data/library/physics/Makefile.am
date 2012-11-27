SUBDIRS = lagrange direct

librarydir = $(pkgdatadir)-@CODYN_API_VERSION@/library/physics
library_DATA = \
	physics.cdn \
	spatial.cdn \
	rnea.cdn \
	crba.cdn \
	dynamics.cdn \
	findbodies.cdn \
	inertia.cdn \
	transform.cdn \
	quaternion.cdn

EXTRA_DIST = \
	$(library_DATA)

-include $(top_srcdir)/git.mk
