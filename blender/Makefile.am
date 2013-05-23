blenderdir = $(pkgdatadir)-@CODYN_API_VERSION@/blender
blender_DATA = \
	__init__.py \
	codyn.py \
	importer.py \
	panel.py \
	simulator.py \
	camera.py

blenderdatadir = $(blenderdir)/data
blenderdata_DATA = \
	data/library.blend \
	data/gui.atlas \
	data/gui.png

blenderzipdir = $(pkgdatadir)-@CODYN_API_VERSION@
blenderzip_DATA = blender_codyn.zip

blender_codyn.zip: $(blender_DATA) $(blenderdata_DATA)
	$(AM_V_GEN) \
	rm -rf blender_codyn $@; \
	mkdir -p blender_codyn/data; \
	cp $(blender_DATA) blender_codyn/; \
	cp -r $(blenderdata_DATA) blender_codyn/data/; \
	zip -r $@ blender_codyn; \
	rm -rf blender_codyn

CLEANFILES = blender_codyn.zip
EXTRA_DIST = $(blender_DATA) $(blenderdata_DATA)

-include $(top_srcdir)/git.mk
