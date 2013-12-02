pages = $(shell find pages -name '*.md' | sed 's/ /\\ /g')
cdnfiles = $(shell find cdn -name '*.cdn' | sed 's/ /\\ /g')

girfile = $(top_builddir)/codyn/Cdn-3.0.gir
#girfile = Cdn-3.0.gir

tikzfigures = $(wildcard figures/*.tikz)
pngfigures = $(patsubst %.tikz,%.png,$(tikzfigures))

javascript = $(wildcard javascript/*.js)
javascript_opts = $(addprefix --custom-js=,$(javascript))

css = $(wildcard styles/*.css)
css_opts = $(addprefix --custom-css=,$(css))

outputloc = html

all: site

site: $(outputloc)/index.html site-figures site-javascript site-styles

site-figures: $(pngfigures)
	$(AM_V_GEN) mkdir -p $(outputloc)/figures;			\
	cp -r $(pngfigures) $(outputloc)/figures/

site-javascript:
	$(AM_V_GEN) mkdir -p $(outputloc)/javascript;			\
	cp -r $(javascript) $(outputloc)/javascript/

site-styles:
	$(AM_V_GEN) mkdir -p $(outputloc)/styles;			\
	cp -r $(css) $(outputloc)/styles/

html/index.html: $(girfile) $(pages) $(cdnfiles) $(pngfigures)
	$(AM_V_GEN) $(CLDOC) gir					\
		--category 'API Reference'				\
		--merge pages						\
		--merge-filter $(srcdir)/generatecode.py		\
		$(javascript_opts)					\
		$(css_opts)						\
		--output $(outputloc)					\
		$(girfile);

serve: $(outputloc)/index.html
	$(CLDOC) serve $(outputloc)

figures/%.pdf: figures/%.tikz
	$(AM_V_GEN) pdflatex -interaction nonstopmode -output-directory figures $< >/dev/null && \
	rm -f $(patsubst %.tikz,%.log,$<) && \
	rm -f $(patsubst %.tikz,%.aux,$<)

figures/%.png: figures/%.pdf
	$(AM_V_GEN) convert $< -background white -flatten $@ && \
	rm -f $<

install-data-hook: $(outputloc)/index.html $(pngfigures)
	@mkdir -p $(DESTDIR)$(docdir)/$(outputloc); \
	cp -r $(outputloc)/index.html $(outputloc)/search.json $(outputloc)/xml $(DESTDIR)$(docdir)/$(outputloc)/; \
	mkdir -p $(DESTDIR)$(docdir)/$(outputloc)/figures; \
	cp $(pngfigures) $(DESTDIR)$(docdir)/$(outputloc)/figures/; \
	mkdir -p $(DESTDIR)$(docdir)/$(outputloc)/javascript; \
	cp $(javascript) $(DESTDIR)$(docdir)/$(outputloc)/javascript/; \
	mkdir -p $(DESTDIR)$(docdir)/$(outputloc)/styles; \
	cp $(css) $(DESTDIR)$(docdir)/$(outputloc)/styles/;

.PHONY: site site-figures site-javascript site-styles
