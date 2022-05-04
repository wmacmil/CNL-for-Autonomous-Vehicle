(TeX-add-style-hook
 "project"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "utf8") ("babel" "english") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")
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
    "beamer"
    "beamer10"
    "fontenc"
    "inputenc"
    "lmodern"
    "babel"
    "verbatim"
    "mathtools"
    "stmaryrd"
    "geometry"
    "setspace"
    "latex/agda"
    "unicode-math"
    "newunicodechar"
    "xcolor"
    "ulem"
    "soul"
    "amsmath"
    "amssymb"
    "multirow"
    "multicol"
    "caption"
    "bussproofs"))
 :latex)

