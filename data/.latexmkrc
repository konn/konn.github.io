$latex            = 'uplatex -file-line-error -interaction=nonstopmode -shell-escape -halt-on-error';
$latex_silent     = 'uplatex -file-line-error -interaction=nonstopmode -shell-escape -halt-on-error';
$bibtex           = 'pbibtex';
$dvipdf           = 'dvipdfmx -p a4 -f otf-up-ipaex.map %O -o %D %S';
$pdflatex         = 'xelatex -file-line-error -interaction=nonstopmode -shell-escape -halt-on-error';
$lualatex         = 'luajittex --fmt=luajitlatex.fmt %O -shell-escape -file-line-error -interaction=nonstopmode -halt-on-error -synctex=1 %S';
$xelatex          = 'xelatex %O -no-pdf -file-line-error -shell-escape %S';
$latex_silent_switch = "-interaction=nonstopmode -halt-on-error";
$pdf_mode         = 1; # generates pdf via pdflatex (XeLaTeX).
$max_repeat       = 5;
$biber            = 'biber --bblencoding=utf8 -u -U --output_safechars';
$dvipdf           = 'dvipdfmx %O -o %D %S';
$makeindex        = 'mendex %O -o %D %S';
$recorder         = true;
