countPeaks <- function(x){

  if (!is.list(x)){
    if (!isMassPeaks(x)){
      stop("x must be a MassPeaks object")}
  }
  
  if (is.list(x)){
    if (!isMassPeaksList(x)){
      stop("x must be a list of MassPeaks objects")}
  }
  
  lengths(x)
  
}
