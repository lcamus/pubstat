## ---- footer
require(htmltools)

tags$footer(style="font-size: 90%",
            hr(),
            p(style="text-align: left; font-style: italic; line-height: 50%;",
              ifelse(params$lang=="FR",
                     "Bas� sur la m�thodologie NACE r�vision 2",
                     "Based on methodology NACE revision 2")),
            tags$ol(class="laparent",style="font-style: italic; padding-left: 3px;",
                    tags$li(HTML(ifelse(params$lang=="FR",
                                        "En plus des salaires bruts des employ�s, les co&ucirc;ts totaux de la main-d'&oelig;uvre incluent les co�ts indirects tels que les cotisations sociales � la charge des employeurs et les imp&ocirc;ts li�s � l'emploi.",
                                        "In addition to employees' gross earnings, total labour costs include indirect costs such as employers' social contributions and taxes connected to the
employment."))),
                    tags$li(HTML(ifelse(params$lang=="FR",
                                        "Ensemble de l'�conomie excluant l'agriculture, la p&ecirc;che et le secteur public",
                                        "Whole economy excluding agriculture, fishing and government sectors.")))
            ),
            p(style="text-align: left;",
              "Source Eurostat",
              span(style="float:right;",
                   paste0(ifelse(params$lang=="FR",
                                 "R�alis� le ",
                                 "Produced on "),
                          format(Sys.Date(),"%e %B %Y")))),
            p(),
            p(class="title", style="text-align: left; color: MidnightBlue; font-size: 100%",
              "Banque de France",
              span(style="float:right; font-style: italic;",
                   HTML(ifelse(params$lang=="FR",
                               "Zone euro &bull; Principaux indicateurs �conomiques et financiers",
                               "The Euro Area &bull; Main economic and financial indicators")))),
            p(),
            p(style="text-align: center; font-weight: bold; color: MidnightBlue;",params$numpage)
)
## ---- end
