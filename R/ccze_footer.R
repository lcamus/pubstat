## ---- footer
require(htmltools)
tags$footer(
  hr(),
  p(style="text-align: right;", paste0("Réalisé le ",format(Sys.Date(),"%d %b %Y"))),
  p(),
  p(
    span(style="text-align: left;","Banque de France"),
    span(style="text-align: right;","Zone euro ● Principaux indicateurs économiques et financiers")
  )

)
## ---- end
