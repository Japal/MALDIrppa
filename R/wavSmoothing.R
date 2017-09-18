wavSmoothing <- function(x, thresh.scale = 2.5, ...){

  if (!isMassSpectrumList(x)) {
    stop("x must be a list of MassSpectrum class objects")
  }
  
  x <- lapply(x, FUN = transfIntensity, fun = function(x) wavShrink(x, thresh.scale = thresh.scale, ...))

  return(x)  

}
