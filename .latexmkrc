#!/usr/bin/env perl
$pdflatex = "luajittex --fmt=luajitlatex.fmt %O -shell-escape -file-line-error -interaction=nonstopmode -halt-on-error -synctex=1 %S";
$pdf_mode = 1;
