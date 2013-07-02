pages = $(wildcard pages/*)
merges = $(foreach i,$(pages),--merge "$(i)")

html/index.html: $(pages) $(top_builddir)/codyn/Cdn-3.0.gir
	$(CLDOC) gir $(merges) --output html $(top_builddir)/codyn/Cdn-3.0.gir

doc_DATA = html/index.html
