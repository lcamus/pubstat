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

o <- Sys.setlocale("LC_TIME",
                   ifelse(params$lang=="FR","French_France.1252","English"))

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

# sep.style <- "box-shadow:-2px 0 0 black;"

css.path <- "../../css/ccze.css"
sep.style <- getStyle(css.path)

