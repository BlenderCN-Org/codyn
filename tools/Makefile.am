SUBDIRS = cdn-monitor cdn-parser cdn-render cdn-compile plugins

if ENABLE_CONTEXT
SUBDIRS += cdn-context
endif

if ENABLE_ARCHIVE
SUBDIRS += cdn-archive
endif

if ENABLE_PYTHON
SUBDIRS += cdn-python cdn-repl
endif

-include $(top_srcdir)/git.mk
