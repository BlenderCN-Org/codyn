SUBDIRS = cdncontext

plugindir = $(GEDIT_PLUGIN_DIR)
plugin_DATA =

if ENABLE_GEDIT3
plugin_DATA += cdncontext.plugin
else
plugin_DATA += cdncontext.gedit-plugin
endif

EXTRA_DIST = \
	cdncontext.plugin.in
	cdncontext.gedit-plugin.in

-include $(top_srcdir)/git.mk
