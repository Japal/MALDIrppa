snrPeaks <-
function(peaks){
  # Extract peaks's SNR values from a list of MassPeaks objects
  lapply(peaks,function(x) x@snr)
}
