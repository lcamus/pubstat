## ---- init

LANG <- "FR"

o <- Sys.setlocale("LC_TIME",
                   ifelse(LANG=="FR","French_France.1252","English"))

suppressMessages(library(highcharter))
library(htmltools)
suppressMessages(library(zoo))

style.color.FR="dodgerblue"
style.color.EA="black"
style.color.IT="limegreen"
style.color.DE="orangered"
style.color.ES="gold"
style.color.GB="darkorchid"
style.color.BE="hotpink"
style.color.NL="darkkhaki"

#set decimal separators and labels export menu for charts
opts <- getOption("highcharter.lang")
opts$decimalPoint <- ifelse(LANG=="FR",",",".")
if (LANG=="FR") {
  opts$contextButtonTitle  <- "Exports & donn&#xE9;es"
  opts$downloadJPEG <- "T&eacute;l&eacute;charger en image JPEG"
  opts$downloadPDF <- "T&eacute;l&eacute;charger en document PDF"
  opts$downloadPNG <- "T&eacute;l&eacute;charger en image PNG"
  opts$downloadSVG <- "T&eacute;l&eacute;charger en image SVG"
  opts$printChart <- "Imprimer"
}
options(highcharter.lang = opts)

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
