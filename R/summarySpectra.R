summarySpectra <- function(x, digits = 4){
  
  if (any(inherits(x,"list") & inherits(x[[1]],"MassSpectrum"))==FALSE) {
    stop("x must be a list of MassSpectrum class objects")
  }
  
  ID <-  names(x)
  NmzVal <- sapply(x,function(x) length(x@mass))
  RmzValmin <- sapply(x,function(x) round(min(x@mass),digits))
  RmzValmax <- sapply(x,function(x) round(max(x@mass),digits))
  RintValmin <- sapply(x,function(x) round(min(x@intensity),digits))
  RintValmean <- sapply(x,function(x) round(mean(x@intensity),digits))
  RintValsd <- sapply(x,function(x) round(sd(x@intensity),digits))
  RintValmed <- sapply(x,function(x) round(median(x@intensity),digits))
  RintValmad <- sapply(x,function(x) round(mad(x@intensity),digits))
  RintValmax <- sapply(x,function(x) round(max(x@intensity),digits))
  out <- data.frame(ID=ID,No.MZ=NmzVal,Min.MZ=RmzValmin,Max.MZ=RmzValmax,Min.Int=RintValmin,Mean.Int=RintValmean,
                    Std.Int=RintValsd,Med.Int=RintValmed,MAD.Int=RintValmad,Max.Int=RintValmax,row.names=NULL)
  
  return(out)
}
