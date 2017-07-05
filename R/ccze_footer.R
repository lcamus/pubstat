## ---- footer
require(htmltools)
tags$footer(
  hr(),
  p(style="text-align: left; font-style: italic;",
    "Basé sur la méthodologie NACE révision 2"),
  p(style="text-align: left;",
    "Source : Eurostat",
    span(style="float:right;",
    paste0("Réalisé le ",format(Sys.Date(),"%d %B %Y")))),
  p(),
  p(style="text-align: left;",
    "Banque de France",
    span(style="float:right; font-style: italic;",
         HTML("Zone euro &bull; Principaux indicateurs économiques et financiers")))
)
## ---- end
