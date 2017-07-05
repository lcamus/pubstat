## ---- init

LANG <- "FR"

o <- Sys.setlocale("LC_TIME",
                   ifelse(LANG=="FR","French_France.1252","English"))

suppressMessages(library(highcharter))
library(htmltools)
suppressMessages(library(zoo))

style.color.FR="dodgerblue"
style.color.EA="black"
style.color.IT="green"
style.color.DE="firebrick"
style.color.ES="gold"
style.color.GB="darkorchid"

#set decimal separators for charts
opts <- getOption("highcharter.lang")
opts$decimalPoint <- ifelse(LANG=="FR",",",".")
options(highcharter.lang = opts)

## ---- end
