SUBDIRS = lagrange direct algorithms

librarydir = $(pkgdatadir)-@CODYN_API_VERSION@/library/physics
library_DATA = \
	contacts.cdn \
	dynamics.cdn \
	findbodies.cdn \
	gravity_compensation.cdn \
	inertia.cdn \
	inverse_dynamics.cdn \
	jacobian.cdn \
	joints.cdn \
	model.cdn \
	physics.cdn \
	quaternion.cdn \
	rendering.cdn \
	sensors.cdn \
	spatial.cdn \
	transform.cdn \
	transformations.cdn

EXTRA_DIST = \
	$(library_DATA)

-include $(top_srcdir)/git.mk
