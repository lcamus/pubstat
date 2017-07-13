## ---- init

o <- Sys.setlocale("LC_TIME",
                   ifelse(params$lang=="FR","French_France.1252","English"))

# suppressMessages(library(zoo))
if (!require("DT")) install.packages('DT')
if (!require("htmltools")) install.packages('htmltools')


## ---- end
