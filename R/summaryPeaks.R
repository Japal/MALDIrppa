summaryPeaks <- function(x, digits = 4){
  
  if (any(inherits(x,"list") & inherits(x[[1]],"MassPeaks"))==FALSE) {
    stop("x must be a list of MassPeaks class objects")
  }
  
  if (!is.null(names(x))) ID <-  names(x) else ID <- 1:length(x)
  NmzVal <- sapply(x,function(x) length(x@mass))
  RmzValmin <- sapply(x,function(x) round(min(x@mass),digits))
  RmzValmax <- sapply(x,function(x) round(max(x@mass),digits))
  RintValmin <- sapply(x,function(x) round(min(x@intensity),digits))
  RintValmean <- sapply(x,function(x) round(mean(x@intensity),digits))
  RintValsd <- sapply(x,function(x) round(sd(x@intensity),digits))
  RintValmed <- sapply(x,function(x) round(median(x@intensity),digits))
  RintValmad <- sapply(x,function(x) round(mad(x@intensity),digits))
  RintValmax <- sapply(x,function(x) round(max(x@intensity),digits))
  RsnrValmin <- sapply(x,function(x) round(min(x@snr),digits))
  RsnrValmean <- sapply(x,function(x) round(mean(x@snr),digits))
  RsnrValsd <- sapply(x,function(x) round(sd(x@snr),digits))
  RsnrValmed <- sapply(x,function(x) round(median(x@snr),digits))
  RsnrValmad <- sapply(x,function(x) round(mad(x@snr),digits))
  RsnrValmax <- sapply(x,function(x) round(max(x@snr),digits))

  out <- data.frame(ID=ID,No.Peaks=NmzVal,Min.MZ=RmzValmin,Max.MZ=RmzValmax,Min.Int=RintValmin,Mean.Int=RintValmean,
                    Std.Int=RintValsd,Med.Int=RintValmed,MAD.Int=RintValmad,Max.Int=RintValmax,
                    Min.SNR=RsnrValmin,Mean.SNR=RsnrValmean,Std.SNR=RsnrValsd,Med.SNR=RsnrValmed,MAD.SNR=RsnrValmad,
                    Max.SNR=RsnrValmax,row.names=NULL)
  
  return(out)
  
}
