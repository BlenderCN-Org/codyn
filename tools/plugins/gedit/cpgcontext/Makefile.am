plugindir = $(GEDIT_PLUGIN_DIR)/cpgcontext

BUILT_SOURCES = \
	viewactivatable.py \
	windowactivatable.py \
	appactivatable.py

plugin_DATA = \
	__init__.py \
	panel.py \
	parser.py \
	selections.py \
	shareddata.py \
	singleton.py \
	basewindowactivatable.py \
	baseviewactivatable.py \
	utils.py \
	$(BUILT_SOURCES)

viewactivatable.py: viewactivatable$(GEDIT_MAJOR_VERSION).py
	cp $(srcdir)/viewactivatable$(GEDIT_MAJOR_VERSION).py $@

windowactivatable.py: windowactivatable$(GEDIT_MAJOR_VERSION).py
	cp $(srcdir)/windowactivatable$(GEDIT_MAJOR_VERSION).py $@

appactivatable.py: appactivatable$(GEDIT_MAJOR_VERSION).py
	cp $(srcdir)/appactivatable$(GEDIT_MAJOR_VERSION).py $@

EXTRA_DIST = \
	$(plugin_DATA) \
	viewactivatable2.py \
	viewactivatable3.py \
	windowactivatable2.py \
	windowactivatable3.py \
	appactivatable3.py \
	appactivatable2.py

CLEANFILES = \
	$(BUILT_SOURCES)

DISTCLEANFILES = \
	$(BUILT_SOURCES)

-include $(top_srcdir)/git.mk
