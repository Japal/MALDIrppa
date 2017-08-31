countPeaks <- function(x){
  
  if (!isMassPeaksList(x)) {
    stop("x must be a list of MassPeaks class objects")
  }
  
  unlist(lapply(x,function(x) length(mass(x))))
}
