## ---- footer
require(htmltools)
tags$footer(style="font-size: 90%",
  hr(),
  p(style="text-align: left; font-style: italic; line-height: 50%;",
    "Basé sur la méthodologie NACE révision 2"),
  tags$ol(class="laparent",style="font-style: italic; padding-left: 3px;",
          tags$li(HTML("En plus des salaires bruts des employés, les co&ucirc;ts totaux de la main-d'&oelig;uvre incluent les coûts indirects tels que les cotisations sociales à la charge des employeurs et les imp&ocirc;ts liés à l'emploi.")),
          tags$li(HTML("Ensemble de l'économie excluant l'agriculture, la p&ecirc;che et le secteur public"))
  ),
  p(style="text-align: left;",
    "Source : Eurostat",
    span(style="float:right;",
         paste0("Réalisé le ",format(Sys.Date(),"%e %B %Y")))),
  p(),
  p(class="title", style="text-align: left; color: MidnightBlue; font-size: 100%",
    "Banque de France",
    span(style="float:right; font-style: italic;",
         HTML("Zone euro &bull; Principaux indicateurs économiques et financiers")))
)
## ---- end
