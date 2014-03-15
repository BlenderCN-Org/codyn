cdnrepldir = $(pythondir)/cdnrepl
cdnrepl_PYTHON =			\
	cdnrepl/__init__.py		\
	cdnrepl/command.py

cdnrepl_DATA =				\
	cdnrepl/plot.html

bin_SCRIPTS = cdn-repl

cdnrepl/plot.html: cdnrepl/plot.html.in cdnrepl/plot.css cdnrepl/plot.js cdnrepl/jquery.js
	$(AM_V_GEN) (cd $(srcdir) && $(PYTHON) make-plot.py)

EXTRA_DIST =				\
	cdnrepl/plot.html		\
	cdnrepl/plot.html.in		\
	cdnrepl/plot.css		\
	cdnrepl/plot.js			\
	cdnrepl/jquery.js

-include $(top_srcdir)/git.mk
