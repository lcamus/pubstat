## ---- init

lang <- "FR"

o <- Sys.setlocale("LC_TIME", 
                   ifelse(lang=="FR","French_France.1252","English"))

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
# hcopts <- getOption("highcharter.options")
# hcopts$lang$decimalPoint <- ifelse(lang=="FR",",",".")
# options(highcharter.options = hcopts)
# print(getOption("highcharter.options"))
opts <- getOption("highcharter.options")
opts$lang$decimalPoint <- ","
options(highcharter.options = opts)

## ---- end
