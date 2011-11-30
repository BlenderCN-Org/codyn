man_MANS = \
	cdn-monitor.1

%.1: %.xml
	@XSLTPROC@ -nonet http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl $<

BUILT_EXTRA_DIST = $(man_MANS)
EXTRA_DIST = cdn-monitor.xml

CLEANFILES = $(man_MANS)

-include $(top_srcdir)/git.mk
