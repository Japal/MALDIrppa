summarySpectra <- function(x, digits = 4){
  
  if (!isMassSpectrumList(x)) {
    stop("x must be a list of MassSpectrum class objects")
  }
  
  if (!is.null(names(x))) ID <-  names(x) else ID <- 1:length(x)
  
  stats <- .summaryStats(x, digits = digits)
  
  return(data.frame(ID = ID, stats, row.names = NULL))
  
}
