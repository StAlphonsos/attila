## -*- makefile -*- for writing with MultiMarkdown

MMD?=multimarkdown
PDFLATEX?=pdflatex
RM?=rm

###
# List your documents here like so:
#
# mydoc.pdf mydoc.html: mydoc.md
#
# Then you can just say e.g.:
#   $ make mydof.pdf

hope.pdf hope.html: hope.md

## The rest of this file makes the above possible...
## ... but only YOU can make it necessary.

.SUFFIXES: .pdf .md .tex .html

.md.tex:
	$(MMD) -t latex -o $@ $<
.tex.pdf:
	$(PDFLATEX) $<
.md.html:
	$(MMD) -t html -o $@ $<

clean:
	$(RM) -f *.{aux,glo,idx,ist,log,toc,out,tex,html}
