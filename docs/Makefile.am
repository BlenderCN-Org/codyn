#girfile = $(top_builddir)/codyn/Cdn-3.0.gir
girfile = Cdn-3.0.gir

tikzfigures = $(wildcard figures/*.tikz)
pngfigures = $(patsubst %.tikz,%.png,$(tikzfigures))

all: html/index.html

html/index.html: $(girfile)
	$(CLDOC) gir --category 'API Reference' --merge pages --output html $(girfile)

serve: html/index.html
	$(CLDOC) serve html

figures/%.pdf: figures/%.tikz
	@echo [GEN] $@; \
	pdflatex -interaction nonstopmode -output-directory figures $< >/dev/null && \
	rm -f $(patsubst %.tikz,%.log,$<) && \
	rm -f $(patsubst %.tikz,%.aux,$<)

figures/%.png: figures/%.pdf
	@echo [GEN] $@; \
	convert $< -background white -flatten $@ && \
	rm -f $<

install-data-hook: html/index.html $(pngfigures)
	@mkdir -p $(DESTDIR)$(docdir)/html; \
	cp -r html/index.html html/search.json html/xml $(DESTDIR)$(docdir)/html/; \
	mkdir -p $(DESTDIR)$(docdir)/html/figures; \
	cp $(pngfigures) $(DESTDIR)$(docdir)/html/figures/;