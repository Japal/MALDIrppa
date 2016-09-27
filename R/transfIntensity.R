transfIntensity <- function(x, fun=NULL, ...){
    
    if ((!inherits(x,"list")) & (!inherits(x,"MassSpectrum"))) stop("x must be a MassSpectrum object")
    if (inherits(x,"list")){
        if (!all(unlist(lapply(x,function(x) inherits(x,"MassSpectrum")))==TRUE)){
          stop("x must be a list of MassSpectrum objects")}
    }
    if (is.null(fun)) stop("A function argument fun must be given")
    
    fun <- match.fun(fun)
   
    if (inherits(x,"list")){
      for (i in 1:length(x)){
        x[[i]]@intensity <- fun(x[[i]]@intensity)
        if (!all(is.finite(x[[i]]@intensity))) x[[i]]@intensity <- rep(0,length(x[[i]]@intensity)) # Deals with flats
      }
    }
    else {x@intensity <- fun(x@intensity)
          if (!all(is.finite(x@intensity))) x@intensity <- rep(0,length(x@intensity))
         }
    
    return(x)
}