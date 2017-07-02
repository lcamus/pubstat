#' @param path path directory containing the data files to import
#' @param template template name of data file to process
#' @return a list compiling the data imported
#' @export
getData <- function(path=".", template="bdf.bsme2.req") {
  
  getFile <- function(f) {
    
    df <- read.table(file=f,header=T,sep="\t",quote="",skip=5,nrows=1)
    df <- read.table(file=f,header=T,sep="\t",quote="",dec=",",na.strings="",
                     colClasses=c("character",rep("numeric",ncol(df)-1)), skip=5)
    df <- setNames(df,gsub("\\.{2,}","\\.",colnames(df)))
    return(df)
    
  }
    
  l.f <- list.files(path,full.names=T)
  l.df <- list()  
  for (f in l.f)
    l.df[[length(l.df)+1]] <- getFile(f)
  # getFile(l.f[1])
  return(l.df)

}
