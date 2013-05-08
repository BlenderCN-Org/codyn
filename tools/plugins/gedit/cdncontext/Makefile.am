plugindir = $(GEDIT_PLUGIN_DIR)/cdncontext

plugin_DATA = \
	__init__.py \
	panel.py \
	parser.py \
	selections.py \
	shareddata.py \
	singleton.py \
	windowactivatable.py \
	viewactivatable.py

EXTRA_DIST = \
	$(plugin_DATA)

CLEANFILES = \
	$(BUILT_SOURCES)

DISTCLEANFILES = \
	$(BUILT_SOURCES)

-include $(top_srcdir)/git.mk
