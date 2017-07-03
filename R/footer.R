## ---- footer
require(htmltools)
tags$footer(
  hr(),
  p(style="text-align: right;", paste0("Réalisé le ",format(Sys.Date(),"%d %b %Y")))
)
## ---- end