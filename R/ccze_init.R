## ---- init

o <- Sys.setlocale("LC_TIME",
                   ifelse(params$lang=="FR","French_France.1252","English"))

suppressMessages(library(highcharter))
library(htmltools)
suppressMessages(library(zoo))

style.color.FR <- "dodgerblue"
style.color.EA <- "black"
style.color.IT <- "limegreen"
style.color.DE <- "orangered"
style.color.ES <- "gold"
style.color.GB <- "darkorchid"
style.color.BE <- "hotpink"
style.color.NL <- "darkkhaki"

style.point.format <- paste0('<span style="color:{point.color}">',
                             "\u25CF",
                             "</span> {series.name}",
                             ifelse(params$lang=="FR"," : ",": "),
                             "<b>{point.y}</b><br/>")

#set decimal separators and labels export menu for charts
opts <- getOption("highcharter.lang")
opts$decimalPoint <- ifelse(params$lang=="FR",",",".")
if (params$lang=="FR") {
  opts$contextButtonTitle  <- "Exports du graphique"
  opts$downloadJPEG <- "T&eacute;l&eacute;charger en image JPEG"
  opts$downloadPDF <- "T&eacute;l&eacute;charger en document PDF"
  opts$downloadPNG <- "T&eacute;l&eacute;charger en image PNG"
  opts$downloadSVG <- "T&eacute;l&eacute;charger en image SVG"
  opts$downloadCSV <- "T&eacute;l&eacute;charger en tableur CSV"
  opts$downloadXLS <- "T&eacute;l&eacute;charger en classeur Excel"
  opts$printChart <- "Imprimer"
}
options(highcharter.lang = opts)

#configuration of export menu in charts
exporting.buttons <- list(contextButton=list(menuItems=list(
  list(textKey='printChart',
       onclick=JS("function(){this.print();}")),
  list(separator=T),
  list(textKey='downloadPNG',
       onclick=JS("function(){this.exportChartLocal();}")),
  list(textKey='downloadPDF',
       onclick=JS("function(){this.exportChartLocal({type: 'application/pdf'});}")),
  list(separator=T),
  list(textKey="downloadCSV",text=opts$downloadCSV,
       onclick=JS("function(){this.downloadCSV();}")),
  list(textKey="downloadXLS",text=opts$downloadXLS,
       onclick=JS("function(){this.downloadXLS();}"))
)))

#JS function to share legend on multiple charts
sharelegend = JS('function(event){
    var vis = this.visible;
                 var conall = $(this.chart.container).parents(".hc-link-legend").find("div.highchart");
                 for(var i = 0; i < conall.length; i++){
                 var hc = $(conall[i]).highcharts();
                 var series = hc.get(this.options.id);
                 if(series){
                 if(vis){
                 series.hide();
                 } else{
                 series.show();
                 }
                 }
                 }
                 return false;
                 }')

## ---- end
