(TeX-add-style-hook
 "roadmap"
 (lambda ()
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "fontspec"
    "fullpage"
    "hyperref"
    "agda"
    "unicode-math"
    "amsfonts"
    "mathtools"
    "xspace"
    "stmaryrd"
    "enumitem"
    "xcolor"
    "newunicodechar")
   (TeX-add-symbols
    '("id" ["argument"] 2)
    '("opp" 1)
    '("indid" 1)
    '("ind" 1)
    '("define" 1)
    '("refl" 1)
    '("pink" 1)
    '("green" 1)
    '("orange" 1)
    '("blue" 1)
    '("gray" 1)
    "jdeq"
    "defeq"
    "blank"
    "UU"
    "rev"
    "bbU"
    "type")
   (LaTeX-add-environments
    "definition"
    "lem"
    "proof"
    "thm")
   (LaTeX-add-bibliographies
    "references")
   (LaTeX-add-xcolor-definecolors
    "asparagus"
    "applegreen"
    "armygreen"
    "calpolypomonagreen"
    "green(ryb)"
    "hooker\\'sgreen"
    "hotmagenta"))
 :latex)

