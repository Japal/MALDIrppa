deletePeaks <- function(x, min = NULL){

  if (!isMassPeaksList(x)) {
    stop("x must be a list of MassPeaks class objects")
  }
  
  if (is.null(min)){
    stop("A minimum peak height value must be given")
  }
  
  lapply(x, function(xx) xx[intensity(xx) >= min])

}
