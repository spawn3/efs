all: study.pdf

study.pdf: study.tex gglocal.sty example.tex daodejing.tex calendar.tex yfs.tex
	pdflatex $<

two:
	pdflatex study.tex


clean:
	rm -rf *.pdf
	rm -rf *.dvi
	rm -rf *.hlog
	rm -rf *.log
	rm -rf *.aux
	rm -rf *.lot
	rm -rf *.lof
	rm -rf *.log
	rm -rf *.toc
