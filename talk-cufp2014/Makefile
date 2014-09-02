wehr_cufp-2014-09.html: talk.txt Makefile
	pandoc --slide-level 2 --highlight-style=pygments -t slidy -s -f markdown -o wehr_cufp-2014-09.html talk.txt

pdf: wehr_cufp-2014-09.pdf

wehr_cufp-2014-09.pdf: talk.txt header.tex body-header.tex *.png
	pandoc --slide-level 2 -s -f markdown -t beamer -H header.tex -B body-header.tex -o wehr_cufp-2014-09.pdf $<
