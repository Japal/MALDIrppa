redResolution <- function(x, by = 1){
    
    if (!is.list(x)){
        if (!isMassSpectrum(x)){
          stop("x must be a MassSpectrum object")}
    }
    
    if (is.list(x)){
        if (!isMassSpectrumList(x)){
          stop("x must be a list of MassSpectrum objects")}
    }
    
    if ((floor(by) != by) || (length(by)!=1)) stop("by must be an integer value")
   
    if (is.list(x)) 
      {x <- lapply(x, function(xx) xx[seq(1, length(xx), by = by)])}
    else
      {x <- x[seq(1, length(x), by = by)]}
    
    return(x)
}