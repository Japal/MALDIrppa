alignPeaks <- function(x, minFreq=0.9, tolerance=0.003, ...){
  
  if (!isMassPeaksList(x)){
      stop("x must be a list of MassPeaks objects")}
  
  refPeaks <- referencePeaks(x, minFrequency = minFreq, method = "strict", tolerance = tolerance)
  warpingFunctions <- determineWarpingFunctions(x, reference = refPeaks, tolerance = tolerance)
  x <- warpMassPeaks(x, warpingFunctions)
  x <- binPeaks(x, method = "strict", tolerance = tolerance)
  x <- binPeaks(x, method = "relaxed", tolerance = tolerance)
  
  return(x)
  
}
