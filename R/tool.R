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

setFooter <- function(o, source=c("","")) {

  if (!require(htmltools)) install.packages("htmltools")

  source.fr <- source[1]
  source.en <- source[2]

  res <- htmltools::withTags(footer(style="font-size: 90%",
                                    p(class="verticalspace"),
                                    hr(class="pub"),
                                    o,
                                    p(),
                                    p(style="text-align: left;",
                                      paste0("Source",ifelse(params$lang=="FR",
                                                             paste0(" : ",source.fr),
                                                             paste0(": ",source.en))),
                                      span(style="float:right;",
                                           paste0(ifelse(params$lang=="FR",
                                                         "Réalisé le ",
                                                         "Produced on "),
                                                  format(Sys.Date(),"%e %B %Y")))),
                                    p(),
                                    p(class="title pub", style="text-align: left; font-size: 100%",
                                      "Banque de France",
                                      span(style="float:right; font-style: italic;",
                                           HTML(ifelse(params$lang=="FR",
                                                       "Zone euro &bull; Principaux indicateurs économiques et financiers",
                                                       "The Euro Area &bull; Main economic and financial indicators")))),
                                    p(),
                                    p(class="pub", style="text-align: center; font-weight: bold;",params$numpage)
  ))

  return(res)

}

#' @param c vector of countries names in FR to translate to EN
#' @return a vector whose names are translated from FR to EN
#' @export
countrynameFR2EN <- function(c,lang=params$lang) {

  if (tolower(lang)=="en") {
    c <- tolower(c)
    c <- sub("^allemagne$","Germany",c)
    c <- sub("^espagne$","Spain",c)
    c <- sub("^zone euro$","Euro area",c)
    c <- sub("^belgique$","Belgium",c)
    c <- sub("^italie$","Italy",c)
    c <- sub("^pays-bas$","Netherlands",c)
    c <- sub("^autriche$","Austria",c)
    c <- sub("^finlande$","Finland",c)
    c <- sub("^grèce$","Greece",c)
    c <- sub("^irlande$","Ireland",c)
    c <- sub("^slovénie$","Slovenia",c)
    c <- sub("^états-unis$","United States",c)
    c <- sub("^japon$","Japan",c)
    c <- sub("^chypre$","Cyprus",c)
    c <- sub("^estonie$","Estonia",c)
    c <- sub("^lettonie$","Latvia",c)
    c <- sub("^lituanie$","Lithuania",c)
    c <- sub("^malte$","Malta",c)
    c <- sub("^slovaquie$","Slovakia",c)
    c <- sub("^bulgarie$","Bulgaria",c)
    c <- sub("^croatie$","Crotia",c)
    c <- sub("^danemark$","Denmark",c)
    c <- sub("^hongrie$","Hungary",c)
    c <- sub("^pologne$","Poland",c)
    c <- sub("^roumanie$","Romania",c)
    c <- sub("^royaume-uni$","United Kingdom",c)
    c <- sub("^suède$","Sweden",c)
    c <- sub("^tchéquie$","Czechia",c)
    c <- sub("^union européenne$","European union",c)
    c <- sub("^japon$","Japan",c)
  } else {
    c <- tolower(c)
    c <- sub("^germany$","Allemagne",c)
    c <- sub("^spain$","Espagne",c)
    c <- sub("^euro area$","Zone euro",c)
    c <- sub("^belgium$","Belgique",c)
    c <- sub("^italy$","Italie",c)
    c <- sub("^netherlands$","Pays-Bas",c)
    c <- sub("^austria$","Autriche",c)
    c <- sub("^finland$","Finlande",c)
    c <- sub("^greece$","Grèce",c)
    c <- sub("^ireland$","Irlande",c)
    c <- sub("^slovenia$","Slovénie",c)
    c <- sub("^united states$","États-Unis",c)
    c <- sub("^japan$","Japon",c)
    c <- sub("^cyprus$","Chypre",c)
    c <- sub("^estonia$","Estonie",c)
    c <- sub("^latvia$","Lettonie",c)
    c <- sub("^lithuania$","Lituanie",c)
    c <- sub("^malta$","Malte",c)
    c <- sub("^slovakia$","Slovaquie",c)
    c <- sub("^bulgaria$","Bulgarie",c)
    c <- sub("^crotia$","Croatie",c)
    c <- sub("^denmark$","Danemark",c)
    c <- sub("^hungary$","Hongrie",c)
    c <- sub("^poland$","Pologne",c)
    c <- sub("^romania$","Roumanie",c)
    c <- sub("^united kingdom$","Royaume-uni",c)
    c <- sub("^sweden$","Suède",c)
    c <- sub("^czechia$","Tchéquie",c)
    c <- sub("^european union$","Union européenne",c)
  }
  c <- stringr::str_to_title(c)
  c <- sub("^Zone Euro$","Zone euro",c)
  c <- sub("^Euro Area$","Euro area",c)
  c <- sub("^Union Européenne$","Union européenne",c)
  c <- sub("^European Union$","European union",c)

  return(c)

}

getCountryByName <- function(c) {

  if (!require(ISOcodes)) install.packages("ISOcodes")
  if (!exists("ISO_3166_1")) data(package="ISOcodes",ISO_3166_1)

  c <- tolower(countrynameFR2EN(c,"EN"))

  res <- sapply(c,function(x){
    if (x=="euro area")
      "EA"
    else if (x=="european union")
      "EU"
    else
      ISO_3166_1[tolower(ISO_3166_1$Name)==x,]$Alpha_2
  })

  return(res)

}

getCountryByCode <- function(c,lang=params$lang) {

  if (!require(ISOcodes)) install.packages("ISOcodes")
  if (!exists("ISO_3166_1")) data(package="ISOcodes",ISO_3166_1)

  c <- tolower(c)
  lang <- tolower(lang)

  res <- sapply(c,function(x){
    if (x=="ea")
      ifelse(lang=="fr","Zone euro","Euro area")
    else if (x=="eu")
      ifelse(lang=="fr","Union européenne","European union")
    else
      countrynameFR2EN(
        ISO_3166_1[tolower(ISO_3166_1$Alpha_2)==x,]$Name,
        lang)
  })

  return(res)

}

highlightTableRowByCountry <- function(country,color,width="1px",begin,end) {

  if (missing(color)) color <- sapply(paste0("style.color.",toupper(country)),get)

  country.lib <- getCountryByCode(country)
  js <- sapply(seq_along(country),function(x){
    stringr::str_interp('if (data[0]=="${country.lib[x]}") {
      $("td",row).css("border-top","${width} solid ${color[x]}");
      $("td",row).css("border-bottom","${width} solid ${color[x]}");
      $("td:eq(${begin})",row).css("border-left","${width} solid ${color[x]}");
      $("td:eq(${end})",row).css("border-right","${width} solid ${color[x]}");
    }\n')
  })
  js <- paste0('function(row,data) {\n',paste0(js,collapse=""),'}\n')

  return(js)

}

genDataTable <- function(data,met,sketch,countries.highlight,nbdigits=1,sep.col=NULL) {

  countries.highlight <- toupper(countries.highlight)
  # countries.highlight.code <- getCountryByName(countries.highlight)
  countries.highlight.name <- getCountryByCode(countries.highlight)
  decimal.sep <- ifelse(params$lang=="FR",",",".")

  # res <- DT::datatable(cbind(country=met[-1],sapply(data[-1,],as.numeric)),
  res <- DT::datatable(cbind(country=met,sapply(data[-1,],as.numeric)),
                       rownames=F, container=sketch,
                       options = list(paging=F,searching=F,info=F,
                                      language=list(decimal=decimal.sep),
                                      columnDefs = list(list(className='dt-right',targets=1:ncol(data),
                                                             defaultContent=ifelse(params$lang=="FR",
                                                                                   "<i>nd</i>","<i>na</i>"))),
                                      rowCallback=DT::JS(
                                        highlightTableRowByCountry(country=countries.highlight,begin=0,end=ncol(data),width="1px")
                                      )
                       ),
                       class="compact hover stripe",escape=F) %>%
    formatCurrency(columns=c(1:ncol(data)+1),currency="",dec.mark=decimal.sep,digits=nbdigits) %>%
    formatStyle(1,target="row",
                fontWeight=styleEqual(countrynameFR2EN(countries.highlight.name),rep("bold",length(countries.highlight.name))),
                color=styleEqual(countrynameFR2EN(countries.highlight.name),
                                 eval(parse(text=sub(",)",")",paste0(
                                   "c(",paste0("style.color.",countries.highlight,",",collapse=""),")"
                                 ))))
                ))

  for (i in length(sep.col))
    res <- res %>% formatStyle(sep.col[i], `box-shadow`='-2px 0 0 black')

  return(res)

}


getTH <- function(variable,liblevel) {

  style.fwn <- "font-weight:normal; "
  style.fwb <- "font-weight:bold; text-align:left; border:none;"
  style.sep <- "box-shadow:-2px 0 0 black;"

  res <- htmltools::withTags(
    if (liblevel=="Y") {
      lapply(
        seq_along(variable),
        function(x){th(
          names(variable)[[x]],colspan=variable[x],
          style=paste0(style.fwb,ifelse(x==1,style.sep,""))
        )})
    } else { # "M" or "Q"
      list(
        lapply(utils::head(variable,1),th,style=paste0(style.fwn,style.sep)),
        lapply(tail(variable,length(variable)-1), th, style=style.fwn)
      )
    }
  )

  return(res)

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
