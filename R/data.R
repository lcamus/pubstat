#' @param path path directory containing the data files to import
#' @param template template name of data file to process
#' @return a list compiling the data imported
#' @export
importData <- function(path=".", template="bdf.bsme2.req") {

  getFile <- function(f,template) {

    if (template!="bdf.bsme2.req")
      l <- list()
    else {

      df_meta <- read.table(file=f,header=T,sep="\t",quote="",skip=3,nrows=2,stringsAsFactors=F)
      df_meta <- setNames(df_meta,c("meta",colnames(df_meta)[2:length(colnames(df_meta))]))
      for (i in 1:2)
        df_meta[i,] <- gsub(" {2,}"," ",df_meta[i,])

      df_data <- read.table(file=f,sep="\t",quote="",dec=",",na.strings="",
                       colClasses=c("character",rep("numeric",ncol(df_meta)-1)), skip=6)
      df_data <- setNames(df_data,c("date",colnames(df_meta)[2:length(colnames(df_meta))]))
      df_data[,]$date <- gsub("/","-",df_data[,]$date)

      l <- list(df_meta,df_data)
      attr(l,"filename") <- f

    }
    return(l)

  }

  l.f <- list.files(path,full.names=T)
  l.data <- list()
  for (f in l.f) {
    l.data[[length(l.data)+1]] <- getFile(f,template)
    # tail(strsplit("data/input/ZEA010BD1.TXT","/")[[1]],1)
  }
  return(l.data)

}

#' @param dc data collection
#' @param dr a data resource which belongs to the data collection
#' @return the data of a data resource
#' @export
getData <- function(dc, dr) {

  return(dc[["dr"]])

}
