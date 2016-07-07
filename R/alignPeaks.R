alignPeaks <-
function(x,minFreq=0.9,tolerance=0.003,...){
  
  # x: list of peaks (massPeaks object)
  # minFreq: minimun frequency for anchor peaks used for alignment
  # tolerance: maximal deviation of a peak position (mass) to be considered as identical

  refPeaks <- referencePeaks(x,minFreq=minFreq,method="strict",tolerance=tolerance)
  warpingFunctions <- determineWarpingFunctions(x, reference=refPeaks,tolerance=tolerance)
  x <- warpMassPeaks(x, warpingFunctions)
  x <- binPeaks(x,method="strict",tolerance=tolerance)
  x <- binPeaks(x,method="relaxed",tolerance=tolerance)
  
  return(x)
}
