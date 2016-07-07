wavSmoothing <-
function(x,thresh.scale=2.5, ...){
  
  # x: list of massSpectrum objects
  # thScale: smoothing factor
  
x <- lapply(x,FUN=transfIntensity,fun=function(x) wavShrink(x,thresh.scale=thresh.scale))
return(x)  
}
