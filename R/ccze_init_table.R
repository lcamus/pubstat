## @knitr init

getStyle <- function(css.file) {

  suppressPackageStartupMessages({if (!require("stringr")) install.packages('stringr')})

  con <- file(css.file, "r")
  text <- readLines(con, 1)
  while(!grepl("^\\.tableverticalseparator", text)) {
    text <- readLines(con, 1)
  }
  close(con)

  pattern <- "(?<={).+(?=})"
  m <- regexpr(pattern,text,perl=T)
  s <- stringr::str_trim(regmatches(text,m))

  return(s)

}

# o <-
invisible(Sys.setlocale("LC_TIME",ifelse(params$lang=="FR","French_France.1252","English")))
# rm(o)

suppressPackageStartupMessages({
  if (!require("DT")) install.packages('DT')
  if (!require("htmltools")) install.packages('htmltools')
  if (!require("zoo")) install.packages('zoo')
  if (!require(ISOcodes)) install.packages("ISOcodes")
})

style.color.FR <- "dodgerblue"
style.color.EA <- "black"
style.color.EU <- "dimgray"
style.color.US <- "navy"
style.color.JP <- "deeppink"
style.color.GB <- "darkorchid"

style.color.USD <- style.color.US
style.color.JPY <- style.color.JP
style.color.GBP <- style.color.GB
style.color.CHF <- "red"
style.color.DKK <- "black"
style.color.SEK <- "black"
style.color.CZK <- "black"
style.color.HUF <- "black"
style.color.RON <- "black"
style.color.BGN <- "black"
style.color.PLN <- "black"

css.path <- "../../css/ccze.css"
sep.style <- getStyle(css.path)
