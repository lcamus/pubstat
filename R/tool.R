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
countrynameFR2EN <- function(c,lang=params$lang) {
  
  # if (missing(lang)) lang <- params$lang
  if (tolower(lang)=="en") {
    c <- tolower(c)
    c <- sub("allemagne","Germany",c)
    c <- sub("espagne","Spain",c)
    c <- sub("zone euro","Euro area",c)
    c <- sub("belgique","Belgium",c)
    c <- sub("italie","Italy",c)
    c <- sub("pays-bas","Netherlands",c)
    c <- sub("autriche","Austria",c)
    c <- sub("finlande","Finland",c)
    c <- sub("grèce","Greece",c)
    c <- sub("irlande","Ireland",c)
    c <- sub("slovénie","Slovenia",c)
  }
  c <- stringr::str_to_title(c)
  c <- sub("Zone Euro","Zone euro",c)
  c <- sub("Euro Area","Euro area",c)

  return(c)
  
}

getCountryByName <- function(c) {
  
  if (!require(ISOcodes)) install.packages("ISOcodes")
  if (!exists("ISO_3166_1")) data(package="ISOcodes",ISO_3166_1)
  
  c <- tolower(countrynameFR2EN(c,"EN"))
  if (c=="euro area")
    res <- "EA"
  else
    res <- ISO_3166_1[tolower(ISO_3166_1$Name)==c,]$Alpha_2
  
  return(res) 
  
}

getCountryByCode <- function(c,lang=params$lang) {
 
  if (!require(ISOcodes)) install.packages("ISOcodes")
  if (!exists("ISO_3166_1")) data(package="ISOcodes",ISO_3166_1)
  
  c <- tolower(c)
  lang <- tolower(lang)
  
  if (c=="ea")
    res <- ifelse(lang=="fr","Zone euro","Euro area")
  else
    res <- countrynameFR2EN(
      ISO_3166_1[tolower(ISO_3166_1$Alpha_2)==c,]$Name,
      lang)
  
  return(res) 
   
}

highlightTableRowByCountry <- function(country,color,width="1px",begin,end) {
  if (missing(color)) color <- get(paste0("style.color.",toupper(country)))
  country.lib <- getCountryByCode(country)
  js <- stringr::str_interp('function(row,data) {
    if (data[0]=="${country.lib}") {
      $("td",row).css("border-top","${width} solid ${color}");
      $("td",row).css("border-bottom","${width} solid ${color}");
      $("td:eq(${begin})",row).css("border-left","${width} solid ${color}");
      $("td:eq(${end})",row).css("border-right","${width} solid ${color}");
    }
  }')
  # cat(js)
  return(js)
  # js <- 'function(row,data) {
  #   if (data[0]=="France") {
  #     $("td",row).css("border-top","1px solid dodgerblue");
  #     $("td",row).css("border-bottom","1px solid dodgerblue");
  #     $("td:eq(0)",row).css("border-left","1px solid dodgerblue");
  #     $("td:eq(16)",row).css("border-right","1px solid dodgerblue");
  #   }
  # }'
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
