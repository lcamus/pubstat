## ---- data

#' @param path path directory containing the data files to import
#' @param template template name of data file to process
#' @return a list compiling the data imported
#' @export
getDataCollection <- function(path=params$data.directory,
                              files=tolower(params$data.collection),
                              templates=tolower(params$data.template),
                              params=tolower(params$data.params)) {

  fixinput <- function(p) {
    if (is.null(p)) p <- rep("",length(files))
    if (length(p)<length(files)) p <- c(p,rep("",length(files)-length(p)))
    return(p)
  }

  getDataResource <- function(f,template,param) {

    if (template=="bdf.bsme2.req") {

      df_meta <- read.table(file=f,header=T,sep="\t",quote="",skip=3,nrows=2,stringsAsFactors=F)
      df_meta <- setNames(df_meta,c("meta",colnames(df_meta)[2:length(colnames(df_meta))]))
      for (i in 1:2)
        df_meta[i,] <- gsub(" {2,}"," ",df_meta[i,])

      df_data <- read.table(file=f,sep="\t",quote="",dec=",",na.strings="",
                            colClasses=c("character",rep("numeric",ncol(df_meta)-1)), skip=6)
      df_data <- setNames(df_data,c("date",colnames(df_meta)[2:length(colnames(df_meta))]))
      df_data[,]$date <- gsub("/","-",df_data[,]$date)

      l <- list(df_meta,df_data)

    }

    else if (template=="bdf.manual.xlsx") {

      if (!require(openxlsx)) install.packages("openxlsx")

      nb.obs <- 13

      df_data <- openxlsx::readWorkbook(f,sheet=param)
      df_data <- df_data[,c(1,seq(from=2,to=ncol(df_data),by=8))]
      df_data$X1 <- format(openxlsx::convertToDate(df_data$X1,origin="1900-01-01"),"%Y-%m")
      df_data <- setNames(df_data,c("date",sub("EL","GR",sapply(strsplit(names(df_data[,-1]),"\\."),`[`,2))))
      df_data <- df_data[df_data$date>="1988-01",]

      calc <- function(f,d) {
        obs.dec <- which(lapply(strsplit(df_data$date,"-"),`[`,2)=="12")
        last.obs <- tail(obs.dec) #to finish
        if ((last.obs)==nrow(d)) last.obs <- last.obs-1
        lapply(lapply(d[,-1],as.numeric),f,na.rm=T)
      }
      df_data.avg <- calc(mean,df_data)
      df_data.sd <- calc(sd,df_data)
      df_data <- (tail(df_data,nb.obs)-df_data.avg)/df_data.std

      df_meta <- names(df_data)

      l <- list(df_meta,df_data)

    }

    else
      l <- list()

    attr(l,"filename") <- f
    return(l)

  }

  templates <- fixinput(templates)
  params <- fixinput(params)

  l.f <- list.files(path,full.names=T, recursive=F)
  l.data <- list()
  for (f in l.f) {
    key <- tolower(tail(strsplit(f,"/")[[1]],1))
    if (key %in% files) {
      index <- which(files %in% key)
      l.data[[key]] <- getDataResource(f,templates[index],params[index])
    }
    else
      next
  }
  return(l.data)

}

#' @param dc data collection
#' @param dr a data resource which belongs to the data collection
#' @return the data of a data resource
#' @export
getData <- function(dc, dr) {

  return(dc[[dr]])

}

## ---- end
