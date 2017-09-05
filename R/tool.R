## ---- tool

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

setLegend <- function(legend=c("",""),suptext=NULL) {

  if (!require(htmltools)) install.packages("htmltools")

  legend.fr <- legend[1]
  legend.en <- legend[2]

  suptext

  res <- htmltools::withTags(
    p(class="unitlegend",
      HTML(ifelse(params$lang=="FR",legend.fr,legend.en)),
      if(!is.null(suptext))
        sup(suptext)
    )
  )

  return(res)

}

setHeader <- function(title, subtitle=c("",""), legend=c("","")) {

  if (!require(htmltools)) install.packages("htmltools")

  title.fr <- title[1]
  title.en <- title[2]

  subtitle.fr <- subtitle[1]
  subtitle.en <- subtitle[2]

  legend.fr <- legend[1]
  legend.en <- legend[2]

  res <- htmltools::withTags(header(
    h3(class="pub title",ifelse(params$lang=="FR",title.fr,title.en)),
    h4(class="pub subtitle",ifelse(params$lang=="FR",subtitle.fr,subtitle.en)),
    hr(class="pub"),
    p(class="unitlegend",ifelse(params$lang=="FR",legend.fr,legend.en)),
    p(class="verticalspace")
  ))

  return(res)

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

currencynameEN2FR <- function(c) {

  c <- tolower(c)
  c <- sub("^us dollar$","Dollar",c)
  c <- sub("^japanese yen$","Yen",c)
  c <- sub("^uk pound sterling$","Livre sterling",c)
  c <- sub("^swiss franc$","Franc suisse",c)
  c <- sub("^danish krone$","Couronne danoise",c)
  c <- sub("^swedish krona$","Couronne suédoise",c)
  c <- sub("^czech koruna$","Couronne tchèque",c)
  c <- sub("^hungarian forint$","Forint hongrois",c)
  c <- sub("^romanian leu$","Leu roumain",c)
  c <- sub("^bulgarian lev$","Lev bulgare",c)
  c <- sub("^polish zloty$","Zloty polonais",c)

  return(c)

}

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
    c <- sub("^république tchèque$","Czechia",c)
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
    else if (x=="usd")
      "Dollar"
    else if (x=="jpy")
      "Yen"
    else if (x=="gbp")
      ifelse(lang=="fr","Livre sterling","Pound Sterling")
    else if (x=="chf")
      ifelse(lang=="fr","Franc suisse","Swiss Franc")
    else if (x=="dkk")
      ifelse(lang=="fr","Couronne danoise","Danish Krone")
    else if (x=="sek")
      ifelse(lang=="fr","Couronne suédoise","Swedish Krona")
    else if (x=="czk")
      ifelse(lang=="fr","Couronne tchèque","Czech Koruna")
    else if (x=="huf")
      ifelse(lang=="fr","Forint hongrois","Hungarian Forint")
    else if (x=="ron")
      ifelse(lang=="fr","Leu roumain","Romanian Leu")
    else if (x=="bgn")
      ifelse(lang=="fr","Lev bulgare","Bulgarian Lev")
    else if (x=="pln")
      ifelse(lang=="fr","Zloty polonais","Polish Zloty")
    else
      countrynameFR2EN(
        ISO_3166_1[tolower(ISO_3166_1$Alpha_2)==x,]$Name,
        lang)
  })

  return(res)

}

customTable <- function(country.name,country.code,color,width="1px",begin,end,
                        subrow=F,countries.name.forced=F,sep.forced=NULL) {

  if (!require(stringr)) install.packages("stringr")

  if (is.null(country.code))
    js <- NULL
  else {

    if (missing(color)) color <- sapply(paste0("style.color.",toupper(country.code)),get)
    if (missing(country.name)) country.name <- getCountryByCode(country.code)

    js <- sapply(seq_along(country.code),function(x){
      stringr::str_interp(paste0(
        'if (data[0]=="${country.name[x]}") {
      $("td",row).css("border-top","${width} solid ${color[x]}");
      $("td",row).css("border-bottom","${width} solid ${color[x]}");
      $("td:eq(${begin})",row).css("border-left","${width} solid ${color[x]}");
      $("td:eq(${end})",row).css("border-right","${width} solid ${color[x]}");',
        ifelse(subrow,'$("td:gt(${begin})",row).html(" ");',''),
        ifelse(subrow | countries.name.forced,'$("td",row).css("font-weight","bold");',''),
        ifelse(subrow | countries.name.forced,'$("td",row).css("color","${color[x]}");',''),
        '}\n'
      ))
    })

    if (!is.null(sep.forced)) {
      js <- c(js,
              # sapply(sep.forced$country.lib,function(x){
              sapply(unique(sep.forced$country.lib),function(x){
                stringr::str_interp(paste0(
                  'if (data[0]=="${x}") {
                         $("td:eq(${sep.forced$col})",row).css("${sep.forced$css.property}","${sep.forced$css.value}");
                  }\n'
                  ))
              }))
      print(js)
      }

    js <- paste0('function(row,data) {\n',paste0(js,collapse=""),'}\n')

  }

  return(js)

}

genDataTable <- function(data,met,sketch,
                         countries.highlight.name,countries.highlight,
                         nbdigits=1,sep.col=NULL,
                         sep.style="box-shadow:-2px 0 0 black;",subrow=F,width=NULL,
                         countries.name.forced=!missing(countries.highlight.name)) {

  if (!require(stringr)) install.packages("stringr")

  if (missing(countries.highlight)) {
    countries.highlight <- c()
    countries.highlight.name <- c()
  } else {
    countries.highlight <- toupper(countries.highlight)
    if (missing(countries.highlight.name))
      countries.highlight.name <- getCountryByCode(countries.highlight)
  }

  decimal.sep <- ifelse(params$lang=="FR",",",".")

  sep.style <- sub(";$","",sep.style)
  sep.style <- strsplit(sep.style,":")[[1]]
  sep.style.cssproperty <- sep.style[1]
  sep.style.cssvalue <- sep.style[2]

  columnDefs <- list(list(className='dt-right',targets=1:ncol(data),
                         defaultContent=ifelse(params$lang=="FR","<i>nd</i>","<i>na</i>")))

  if (!is.null(width))
    columnDefs[[2]] <- width

  #get around seeming issue when formatStyle applied to two consecutive cells with defaultContent (na values):
  sep.forced <- NULL
  if (!is.null(sep.col)) {
    df <- as.data.frame(which(is.na(data) | data=="",arr.ind=T,useNames=F))
    if (nrow(df) != 0) {
      df <- setNames(df,c("row","col"))
      sep.forced <- list(
        country.lib=met[as.numeric(rownames(table(df[df$col %in% c(sep.col-1,sep.col),])))-1],
        col=sep.col-1,
        css.property=sep.style.cssproperty,
        css.value=sep.style.cssvalue
      )
      # print(df[df$col %in% c(sep.col-1,sep.col),])
      # print(df$col)
      print(sep.forced)
      rm(df)
    }
  }

res <- DT::datatable(cbind(country=met,sapply(data,as.numeric)),
                     rownames=F, container=sketch,
                     options = list(paging=F,searching=F,info=F,
                                    language=list(decimal=decimal.sep),
                                    columnDefs = columnDefs,
                                    rowCallback=DT::JS(
                                      customTable(country.name=countries.highlight.name,country.code=countries.highlight,
                                                  begin=0,end=ncol(data),width="1px",
                                                  subrow=subrow,countries.name.forced=countries.name.forced,sep.forced=sep.forced)
                                    )
                     ),
                     class="compact hover stripe",escape=F) %>%
  formatCurrency(columns=c(1:ncol(data)+1),currency="",dec.mark=decimal.sep,digits=nbdigits)

if (!subrow & !is.null(countries.highlight) & !countries.name.forced)
  res <- res %>%
  formatStyle(1,target="row",
              fontWeight=styleEqual(countrynameFR2EN(countries.highlight.name),rep("bold",length(countries.highlight.name))),
              color=styleEqual(countrynameFR2EN(countries.highlight.name),
                               eval(parse(text=sub(",)",")",paste0(
                                 "c(",paste0("style.color.",countries.highlight,",",collapse=""),")"
                               ))))
              ))

res <- res %>% formatStyle(sep.col, `box-shadow`=sep.style.cssvalue)

return(res)

}


getTH <- function(variable,liblevel,sep.style="", base.style) {

  style.fwn <- "font-weight:normal; "
  style.fwb <- "font-weight:bold; text-align:left; border:none;"

  if (missing(base.style))
    if (liblevel=="Y")
      base.style <- style.fwb
    else
      base.style <- style.fwn

  res <- htmltools::withTags(
    if (liblevel=="Y") {
      lapply(
        seq_along(variable),
        function(x){th(
          names(variable)[[x]],colspan=variable[x],
          style=paste0(base.style,ifelse(x==1,sep.style,""))
        )})
    } else { # "M" or "Q"
      list(
        lapply(utils::head(variable,1),th,style=paste0(base.style,sep.style)),
        lapply(tail(variable,length(variable)-1), th, style=base.style)
      )
    }
  )

  return(res)

}





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
