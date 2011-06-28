SUBDIRS = cpgcontext

plugindir = $(GEDIT_PLUGIN_DIR)
plugin_DATA =

if ENABLE_GEDIT3
plugin_DATA += cpgcontext.plugin
else
plugin_DATA += cpgcontext.gedit-plugin
endif

EXTRA_DIST = \
	cpgcontext.plugin.in
	cpgcontext.gedit-plugin.in

-include $(top_srcdir)/git.mk
