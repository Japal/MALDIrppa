wavSmoothing <- function(x, method=c("Wavelet","SavitzkyGolay","MovingAverage"), n.levels = 4, ...){
  
  if (!isMassSpectrumList(x)) {
    stop("x must be a list of MassSpectrum class objects")
  }
  
  method <- match.arg(method)

  wavShrink <- function(x, n.levels = n.levels, ...){
    
    if(n.levels > log(length(x),2)) {stop("n.levels must be <= log(length(x),2)")}
    
    t <- waveslim::modwt(x, boundary = "reflection") # arguments based on original wavShrink from wmtsa
    t.thresh <- waveslim::universal.thresh.modwt(t, max.level = n.levels)
    ti <- waveslim::imodwt(t.thresh)
    }
  
  if (method=="Wavelet"){
    x <- lapply(x, FUN = transfIntensity, fun = function(x) wavShrink(x, n.levels = n.levels, ...))
  }  
  if (method=="SavitzkyGolay") {x <- MALDIquant::smoothIntensity(x, method="SavitzkyGolay", ...)}
  if (method=="MovingAverage") {x <- MALDIquant::smoothIntensity(x, method="MovingAverage", ...)}

  return(x)  
  
}

