wavSmoothing <- function(x, method=c("Wavelet","SavitzkyGolay","MovingAverage"), thresh.scale = 2.5, ...){

  if (!isMassSpectrumList(x)) {
    stop("x must be a list of MassSpectrum class objects")
  }
  
  method <- match.arg(method)
  
  if (method=="Wavelet"){
    x <- lapply(x, FUN = transfIntensity, fun = function(x) wmtsa::wavShrink(x, thresh.scale = thresh.scale, ...))
  }
  
  if (method=="SavitzkyGolay") {x <- MALDIquant::smoothIntensity(x, method="SavitzkyGolay", ...)}
  if (method=="MovingAverage") {x <- MALDIquant::smoothIntensity(x, method="MovingAverage", ...)}

  return(x)  

}
