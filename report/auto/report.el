(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("biblatex" "mincrossrefs=999" "style=numeric" "backend=biber" "url=false" "isbn=false" "doi=false" "") ("geometry" "margin=1in") ("xcolor" "dvipsnames") ("hyperref" "colorlinks") ("pstricks" "usenames" "dvipsnames") ("grffile" "space")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "biblatex"
    "geometry"
    "xcolor"
    "hyperref"
    "enumitem"
    "amsfonts"
    "unicode-math"
    "stmaryrd"
    "mathtools"
    "xspace"
    ""
    "fontspec"
    "tikz-cd"
    "titlesec"
    "graphicx"
    "float"
    "pstricks"
    "epsfig"
    "pst-grad"
    "pst-plot"
    "grffile"
    "etoolbox")
   (LaTeX-add-labels
    "fig:A1"
    "fig:A2"
    "fig:N2"
    "fig:M1"
    "fig:M2"
    "fig:M3")
   (LaTeX-add-bibliographies
    "references")
   (LaTeX-add-xparse-macros
    '("codeword" "v")
    '("term" "v")
    '("keyword" "v")))
 :latex)

