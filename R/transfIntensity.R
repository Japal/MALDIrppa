transfIntensity <- function(x, fun = NULL, ...){
    
    if (!is.list(x)){
      if (!isMassSpectrum(x)){
        stop("x must be a MassSpectrum object")}
    }
    
    if (is.list(x)){
      if (!isMassSpectrumList(x)){
        stop("x must be a list of MassSpectrum objects")}
    }
    
    if (is.null(fun)) stop("A function argument fun must be given")
    
    fun <- match.fun(fun)
   
    if (is.list(x)){
      for (i in 1:length(x)){
        intensity(x[[i]]) <- fun(intensity(x[[i]]))
        if (!all(is.finite(intensity(x[[i]])))) intensity(x[[i]]) <- rep(0, length(intensity(x[[i]]))) # Deals with flats
      }
    }
    else {intensity(x) <- fun(intensity(x))
          if (!all(is.finite(intensity(x)))) intensity(x) <- rep(0, length(intensity(x)))
         }
    
    return(x)

}