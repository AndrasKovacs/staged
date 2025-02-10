#
# Makefile for acmart package
#
# This file is in public domain
#
# $Id: Makefile,v 1.10 2016/04/14 21:55:57 boris Exp $
#

PACKAGE=acmart

DEV=-dev # To switch dev on
#DEV=

PDF = $(PACKAGE).pdf acmguide.pdf

BIBLATEXFILES= $(wildcard *.bbx) $(wildcard *.cbx) $(wildcard *.dbx) $(wildcard *.lbx)

SAMPLEBIBLATEXFILES=$(patsubst %,samples/%,$(BIBLATEXFILES))

ACMCPSAMPLES= \
	samples/sample-acmcp-Discussion.pdf \
	samples/sample-acmcp-Invited.pdf \
	samples/sample-acmcp-Position.pdf \
	samples/sample-acmcp-Research.pdf \
	samples/sample-acmcp-Review.pdf 

all:  ${PDF} ALLSAMPLES

%.pdf:  %.dtx   $(PACKAGE).cls
	pdflatex $<
	- bibtex $*
	pdflatex $<
	- makeindex -s gind.ist -o $*.ind $*.idx
	- makeindex -s gglo.ist -o $*.gls $*.glo
	pdflatex $<
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $*.log) \
	do pdflatex $<; done


acmguide.pdf: $(PACKAGE).dtx $(PACKAGE).cls
	pdflatex -jobname acmguide $(PACKAGE).dtx
	- bibtex acmguide
	pdflatex -jobname acmguide $(PACKAGE).dtx
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' acmguide.log) \
	do pdflatex -jobname acmguide $(PACKAGE).dtx; done

%.cls:   %.ins %.dtx
	pdflatex $<

%-tagged.cls:   %.ins %.dtx
	pdflatex $<


ALLSAMPLES: $(SAMPLEBIBLATEXFILES)
	cd samples; pdflatex samples.ins; cd ..
	for texfile in samples/*.tex; do \
		pdffile=$${texfile%.tex}.pdf; \
		${MAKE} $$pdffile; \
	done

samples/%: %
	cp $^ samples


samples/$(PACKAGE).cls: $(PACKAGE).cls
samples/$(PACKAGE)-tagged.cls: $(PACKAGE)-tagged.cls
samples/ACM-Reference-Format.bst: ACM-Reference-Format.bst

samples/abbrev.bib: ACM-Reference-Format.bst
	perl -pe 's/MACRO ({[^}]*}) *\n/MACRO \1/' ACM-Reference-Format.bst \
	| grep MACRO | sed 's/MACRO {/@STRING{/' \
	| sed 's/}  *{/ = /' > samples/abbrev.bib 


samples/%.bbx: %.bbx
samples/%.cbx: %.cbx
samples/%.dbx: %.dbx
samples/%.lbx: %.lbx



samples/%.pdf:  samples/%.tex   samples/$(PACKAGE).cls samples/ACM-Reference-Format.bst
	cd $(dir $@) && pdflatex${DEV} $(notdir $<)
	- cd $(dir $@) && bibtex $(notdir $(basename $<))
	cd $(dir $@) && pdflatex${DEV} $(notdir $<)
	cd $(dir $@) && pdflatex${DEV} $(notdir $<)
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $(basename $<).log) \
	  do cd $(dir $@) && pdflatex${DEV} $(notdir $<); done

samples/sample-sigconf-biblatex.pdf: samples/sample-sigconf-biblatex.tex $(SAMPLEBIBLATEXFILES)
	cd $(dir $@) && pdflatex${DEV} $(notdir $<)
	- cd $(dir $@) && biber $(notdir $(basename $<))
	cd $(dir $@) && pdflatex${DEV} $(notdir $<)
	cd $(dir $@) && pdflatex${DEV} $(notdir $<)
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $(basename $<).log) \
	  do cd $(dir $@) && pdflatex${DEV} $(notdir $<); done

samples/sample-acmsmall-biblatex.pdf: samples/sample-acmsmall-biblatex.tex $(SAMPLEBIBLATEXFILES)
	cd $(dir $@) && pdflatex${DEV} $(notdir $<)
	- cd $(dir $@) && biber $(notdir $(basename $<))
	cd $(dir $@) && pdflatex${DEV} $(notdir $<)
	cd $(dir $@) && pdflatex${DEV} $(notdir $<)
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $(basename $<).log) \
	  do cd $(dir $@) && pdflatex${DEV} $(notdir $<); done

samples/sample-sigconf-xelatex.pdf:  samples/sample-xelatex.tex   samples/$(PACKAGE).cls samples/ACM-Reference-Format.bst
	cd $(dir $@) && xelatex${DEV} $(notdir $<)
	- cd $(dir $@) && bibtex $(notdir $(basename $<))
	cd $(dir $@) && xelatex${DEV} $(notdir $<)
	cd $(dir $@) && xelatex${DEV} $(notdir $<)
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $(basename $<).log) \
	  do cd $(dir $@) && xelatex${DEV} $(notdir $<); done

samples/sample-sigconf-lualatex.pdf:  samples/sample-lualatex.tex   samples/$(PACKAGE).cls samples/ACM-Reference-Format.bst
	cd $(dir $@) && lualatex${DEV} $(notdir $<)
	- cd $(dir $@) && bibtex $(notdir $(basename $<))
	cd $(dir $@) && lualatex${DEV} $(notdir $<)
	cd $(dir $@) && lualatex${DEV} $(notdir $<)
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $(basename $<).log) \
	  do cd $(dir $@) && lualatex${DEV} $(notdir $<); done

samples/sample-acmcp.pdf: samples/acm-jdslogo.png


samples/sample-acmsmall-tagged.pdf:  samples/sample-acmsmall-tagged.tex   samples/$(PACKAGE)-tagged.cls samples/ACM-Reference-Format.bst
	cd $(dir $@) && lualatex-dev $(notdir $<)
	- cd $(dir $@) && bibtex $(notdir $(basename $<))
	cd $(dir $@) && lualatex-dev $(notdir $<)
	cd $(dir $@) && lualatex-dev $(notdir $<)
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $(basename $<).log) \
	  do cd $(dir $@) && lualatex-dev $(notdir $<); done



.PRECIOUS:  $(PACKAGE).cfg $(PACKAGE).cls $(PACKAGE)-tagged.cls

docclean:
	$(RM)  *.log *.aux \
	*.cfg *.glo *.idx *.toc \
	*.ilg *.ind *.out *.lof \
	*.lot *.bbl *.blg *.gls *.cut *.hd \
	*.dvi *.ps *.thm *.tgz *.zip *.rpi \
	samples/$(PACKAGE).cls \
	samples/$(PACKAGE)-tagged.cls \
	samples/ACM-Reference-Format.bst \
	samples/*.log samples/*.aux samples/*.out \
	samples/*.bbl samples/*.blg samples/*.cut samples/*.toc \
	samples/acm-jdslogo.png \
	samples/*.run.xml samples/*.bcf $(SAMPLEBIBLATEXFILES)


clean: docclean
	$(RM)  $(PACKAGE).cls $(PACKAGE)-tagged.cls \
	samples/*.tex

distclean: clean
	$(RM)  *.pdf samples/sample-*.pdf

#
# Archive for the distribution. Includes typeset documentation
#
archive:  all clean
	COPYFILE_DISABLE=1 tar -C .. -czvf ../$(PACKAGE).tgz --exclude '*~' --exclude '*.tgz' --exclude '*.zip'  --exclude CVS --exclude '.git*' $(PACKAGE); mv ../$(PACKAGE).tgz .

zip:  all clean
	zip -r  $(PACKAGE).zip * -x '*~' -x '*.tgz' -x '*.zip' -x CVS -x 'CVS/*'

# distros
distros: all docclean
	zip -r acm-distro.zip  \
	acmart.pdf acmguide.pdf samples *.cls ACM-Reference-Format.* \
	--exclude samples/sample-acmengage*
	zip -r acmengage-distro.zip samples/sample-acmengage* \
	samples/*.bib \
	acmart.pdf acmguide.pdf  *.cls ACM-Reference-Format.*

acmcp.zip: ${ACMCPSAMPLES} acmart.cls
	zip $@ $+

samples/sample-acmcp.tex: samples/samples.ins samples/samples.dtx
	cd samples; pdflatex samples.ins; cd ..


samples/sample-acmcp-%.tex: samples/sample-acmcp.tex samples/acm-jdslogo.png
	sed 's/acmArticleType{Review}/acmArticleType{$*}/' $< > $@

.PHONY: all ALLSAMPLES docclean clean distclean archive zip
