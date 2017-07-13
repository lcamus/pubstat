## ---- footer
require(htmltools)

tags$footer(style="font-size: 90%",
            hr(),
            p(style="text-align: left; font-style: italic; line-height: 50%;",
              ifelse(params$lang=="FR",
                     "Basé sur la méthodologie NACE révision 2",
                     "Based on methodology NACE revision 2")),
            tags$ol(class="laparent",style="font-style: italic; padding-left: 3px;",
                    tags$li(HTML(ifelse(params$lang=="FR",
                                        "En plus des salaires bruts des employés, les co&ucirc;ts totaux de la main-d'&oelig;uvre incluent les coûts indirects tels que les cotisations sociales à la charge des employeurs et les imp&ocirc;ts liés à l'emploi.",
                                        "In addition to employees' gross earnings, total labour costs include indirect costs such as employers' social contributions and taxes connected to the
employment."))),
                    tags$li(HTML(ifelse(params$lang=="FR",
                                        "Ensemble de l'économie excluant l'agriculture, la p&ecirc;che et le secteur public",
                                        "Whole economy excluding agriculture, fishing and government sectors.")))
            ),
            p(style="text-align: left;",
              "Source Eurostat",
              span(style="float:right;",
                   paste0(ifelse(params$lang=="FR",
                                 "Réalisé le ",
                                 "Produced on "),
                          format(Sys.Date(),"%e %B %Y")))),
            p(),
            p(class="title", style="text-align: left; color: MidnightBlue; font-size: 100%",
              "Banque de France",
              span(style="float:right; font-style: italic;",
                   HTML(ifelse(params$lang=="FR",
                               "Zone euro &bull; Principaux indicateurs économiques et financiers",
                               "The Euro Area &bull; Main economic and financial indicators")))),
            p(),
            p(style="text-align: center; font-weight: bold; color: MidnightBlue;",params$numpage)
)
## ---- end
