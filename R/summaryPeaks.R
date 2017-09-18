summaryPeaks <- function(x, digits = 4){
  
  if (!isMassPeaksList(x)) {
    stop("x must be a list of MassPeaks class objects")
  }
  
  if (!is.null(names(x))) ID <-  names(x) else ID <- 1:length(x)
  
  stats <- .summaryStats(x, digits = digits)
  
  Min.SNR <- sapply(x, function(x) round(min(snr(x)), digits))
  Mean.SNR <- sapply(x, function(x) round(mean(snr(x)), digits))
  Std.SNR <- sapply(x, function(x) round(sd(snr(x)), digits))
  Med.SNR <- sapply(x, function(x) round(median(snr(x)), digits))
  MAD.SNR <- sapply(x, function(x) round(mad(snr(x)), digits))
  Max.SNR <- sapply(x, function(x) round(max(snr(x)), digits))

  return(data.frame(ID = ID, stats, 
                    Min.SNR, Mean.SNR, Std.SNR, Med.SNR, MAD.SNR, Max.SNR,
                    row.names = NULL))
  
}
