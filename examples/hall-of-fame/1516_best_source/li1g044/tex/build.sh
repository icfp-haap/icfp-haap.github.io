#!/bin/bash

pdflatex -shell-escape relatorio.tex &&
pdflatex -shell-escape relatorio.tex &&
pdflatex -shell-escape relatorio.tex &&
xdg-open relatorio.pdf
