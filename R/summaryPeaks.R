summaryPeaks <- function(x, ncases = NULL, ...){
  
  if (any(inherits(x,"list") & inherits(x[[1]],"MassPeaks"))==FALSE) {
    stop("x must be a list of MassPeaks class objects")
  }
  
  if (inherits(ncases,"numeric")==FALSE) {
    stop("ncases must be a valid number")
  }
  
  if (is.null(names(x))) {ID <- 1:length(x)} else {ID <-  names(x)}
  NmzVal <- sapply(x,function(x) length(x@mass))
  RmzValmin <- sapply(x,function(x) round(min(x@mass),2))
  RmzValmax <- sapply(x,function(x) round(max(x@mass),2))
  RintValmin <- sapply(x,function(x) round(min(x@intensity),2))
  RintValmean <- sapply(x,function(x) round(mean(x@intensity),2))
  RintValsd <- sapply(x,function(x) round(sd(x@intensity),2))
  RintValmed <- sapply(x,function(x) round(median(x@intensity),2))
  RintValmad <- sapply(x,function(x) round(mad(x@intensity),2))
  RintValmax <- sapply(x,function(x) round(max(x@intensity),2))
  RsnrValmin <- sapply(x,function(x) round(min(x@snr),2))
  RsnrValmean <- sapply(x,function(x) round(mean(x@snr),2))
  RsnrValsd <- sapply(x,function(x) round(sd(x@snr),2))
  RsnrValmed <- sapply(x,function(x) round(median(x@snr),2))
  RsnrValmad <- sapply(x,function(x) round(mad(x@snr),2))
  RsnrValmax <- sapply(x,function(x) round(max(x@snr),2))

  out <- data.frame(ID=ID,No.Peaks=NmzVal,Min.MZ=RmzValmin,Max.MZ=RmzValmax,Min.Int=RintValmin,Mean.Int=RintValmean,
                    Std.Int=RintValsd,Med.Int=RintValmed,MAD.Int=RintValmad,Max.Int=RintValmax,
                    Min.SNR=RsnrValmin,Mean.SNR=RsnrValmean,Std.SNR=RsnrValsd,Med.SNR=RsnrValmed,MAD.snr=RsnrValmad,
                    Max.SNR=RsnrValmax,row.names=NULL)
  
  if (is.null(ncases)) {return(out)} else {return(head(out,ncases))} 
  
}
