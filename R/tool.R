## ---- tool

#' @param s string to convert to HTML-entities
#' @return a string whose special characters are converted to HTML entities
#' @export
stringToHtmlEntities <- function(s) {

  f_he.src <- "../../data/html-entities.xlsx"
  f_he <- "../../data/html-entities.RData"
 #source: https://www.freeformatter.com/html-entities.html
  if (!exists("html.entities")) {
    if (file.exists(f_he))
      load(f_he)
    else {
      html.entities <- readxl::read_xlsx(path=f_he.src, range="A1:D331")
      save(html.entities,file=f_he)
    }
  }

  he <- html.entities[!is.na(html.entities[,c("Entity Name")]) & !is.na(html.entities$Character),]
  s <- gsub("&","&amp;",s)
  for (i in 2:nrow(he))
    s <- gsub(he[i,]$Character,he[i,c("Entity Name")],s)

  return(s)

}

#' @param c vector of countries names in FR to translate to EN
#' @return a vector whose names are translated from FR to EN
#' @export
countrynameFR2EN <- function(c) {
  if (params$lang=="EN") {
    c <- sub("Allemagne","Germany",c)
    c <- sub("Espagne","Spain",c)
    c <- sub("Zone euro","Euro area",c)
    c <- sub("Belgique","Belgium",c)
    c <- sub("Italie","Italy",c)
    c <- sub("Pays-Bas","Netherlands",c)
    c <- sub("Autriche","Austria",c)
    c <- sub("Finlande","Finland",c)
    c <- sub("Grèce","Greece",c)
    c <- sub("Irlande","Ireland",c)
    c <- sub("Slovénie","Slovenia",c)
  }
  return(c)
}

#' #' @param s string to convert to HTML-entities
#' #' @return a string whose special characters are converted to HTML entities
#' #' @export
#' convertToPDF <- function (f.in, f.out, p) {
#'   print(getwd())
#'   system(paste0("cmd /c phantomjs ",
#'                 "\\js\\rasterize_bdf.js ",
#'                 f.in,
#'                 " ",
#'                 f.out,
#'                 " ",
#'                 p
#'                 ))
#' }

## ---- end
