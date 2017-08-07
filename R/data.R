## ---- data

getDataCollection <- function(path=params$data.directory,
                              files=params$data.collection,
                              templates=params$data.template,
                              parameters=params$data.params) {

  #set input YAML parameters to R objects
  args <- as.list(environment())
  for (i in seq_along(args)) {
    # t <- eval(parse(text=paste0('sub("^!r","",args[[',i,']])')))
    try(assign(names(args)[i],
               eval(parse(text=eval(parse(text=paste0('sub("^!r","",args[[',i,']])')))))),
        silent=T)
  }

  fixinput <- function(p) {
    if (is.null(p)) p <- rep("",length(files))
    if (length(p)<length(files)) p <- c(p,rep("",length(files)-length(p)))
    return(p)
  }

  getDataResource <- function(f,template,param,wsEntryPoint) {

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

      suppressMessages(if (!require(openxlsx)) install.packages("openxlsx"))

      nb.obs <- 13

      df_data <- openxlsx::readWorkbook(f,sheet=param)
      df_data <- df_data[,c(1,seq(from=2,to=ncol(df_data),by=8))]
      df_data$X1 <- format(openxlsx::convertToDate(df_data$X1,origin="1900-01-01"),"%Y-%m")
      df_data <- setNames(df_data,c("date",
                                    sub("UK","GB",
                                        sub("EL","GR",
                                            sapply(strsplit(names(df_data[,-1]),"\\."),`[`,2))
                                    )))
      df_data <- df_data[df_data$date>="1988-01",]
      df_data[,-1] <- lapply(df_data[,-1],as.numeric)

      calc <- function(f,d) {
        obs.dec <- which(lapply(strsplit(d$date,"-"),`[`,2)=="12")
        last.obs <- ifelse(tail(obs.dec,1)==nrow(d),obs.dec[length(obs.dec)-1],tail(obs.dec,1))
        lapply(d[1:last.obs,-1],f,na.rm=T)
      }
      df_data.avg <- calc(mean,df_data)
      df_data.std <- calc(sd,df_data)
      df_data <- tail(df_data,nb.obs)
      df_data[,-1] <- (tail(df_data[,-1],nb.obs)-df_data.avg)/df_data.std

      df_meta <- names(df_data)

      l <- list(df_meta,df_data)

    }

    else if (template=="ecb.sdw.ws") {

      suppressMessages({
        if (!require(httr)) install.packages("httr")
        if (!require(readr)) install.packages("readr")
      })

      setUrl <- function(protocol="http",wsEntryPoint,resource="data",flowRef,key,parameters,dim) {

        #get protocol from wsEntryPoint if supplied and override protocol dedicated field value
        protocol.wsEntrypoint <- regmatches(wsEntryPoint,regexec("^.+(?=://)",wsEntryPoint,perl=T))[[1]]
        if (length(protocol.wsEntrypoint)>0) {
          protocol <- protocol.wsEntrypoint
          wsEntryPoint <- sub(paste0(protocol,"://"),"",wsEntryPoint)
          rm(protocol.wsEntrypoint)
          wsEntryPoint <- sub("/$","",wsEntryPoint)
        }

        #set SDMX key dimensions
        key.sdmx <- ""
        for (i in dim) {
          if (i %in% names(key)) {
            key.sdmx <- paste0(key.sdmx,paste0(key[[i]],collapse="+"),".")
          }
          else
            key.sdmx <- paste0(key.sdmx,".")
        }
        key <- sub("\\.$","",key.sdmx)
        rm(key.sdmx)

        #set SDMX parameters
        parameters <- paste0(lapply(seq_along(parameters),function(x){paste0(names(parameters)[x],"=",parameters[[x]])}),collapse="&")

        res <- paste0(protocol,"://",paste(wsEntryPoint,resource,flowRef,key,sep="/"),"?",parameters)
        return(res)

      }

      url <- setUrl(wsEntryPoint=wsEntryPoint,
                    flowRef=f$DATASET,
                    key=f[-which(c("DATASET","dimensions") %in% names(f))], #only SDMX dimensions
                    parameters=param,
                    dim=f$dimensions)

      response <- httr::GET(url,httr::accept("text/csv"))
      #http://sdw-wsrest.ecb.int/service/data/EXR/A.BGN.EUR.SP00.?startPeriod=1999&endPeriod=1999
      response <- readr::read_csv(httr::content(response,"text",encoding="UTF-8"))

      df_data <- response[,1:8]
      df_data$date <- df_data$TIME_PERIOD
      df_data$TIME_PERIOD <- NULL

      df_meta <- response[,c("TITLE","TITLE_COMPL")]

      l <- list(df_meta,df_data)

    }

    else
      l <- list()

    attr(l,"filename") <- f
    return(l)

  }

  templates <- fixinput(templates)
  parameters <- fixinput(parameters)

  l.data <- list()
  if (any(unique(templates) %in% c("bdf.bsme2.req","bdf.manual.xlsx"))) {
    l.f <- list.files(path,full.names=T, recursive=F)
    for (f in l.f) {
      key <- tolower(tail(strsplit(f,"/")[[1]],1))
      if (key %in% files) {
        index <- which(files %in% key)
        l.data[[key]] <- getDataResource(f,templates[index],parameters[index])
      }
      else
        next
    }
  } else # ecb.sdw.ws
    for (f in seq_along(files))
      l.data[[f]] <- getDataResource(files[[f]],templates[f],parameters[[f]],wsEntryPoint=params$data.directory)

  return(l.data)

}

getData <- function(dc, dr) {
  return(dc[[dr]])

}

## ---- end
