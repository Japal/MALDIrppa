snrPeaks <- function(x){

  if (!isMassPeaksList(x)) {
    stop("x must be a list of MassPeaks class objects")
  }
  
  lapply(x,function(x) snr(x))

}
