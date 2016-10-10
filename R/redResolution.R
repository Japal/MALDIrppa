redResolution <- function(x, by = 1){
    
    if ((!inherits(x,"list")) & (!inherits(x,"MassSpectrum"))) stop("x must be a MassSpectrum object")
    if (inherits(x,"list")){
        if (!all(unlist(lapply(x,function(x) inherits(x,"MassSpectrum")))==TRUE)){
          stop("x must be a list of MassSpectrum objects")}
    }
    if ((floor(by) != by) | (length(by)!=1)) stop("by must be an integer value")
   
    if (inherits(x,"list")){
      for (i in 1:length(x)){
        x[[i]]@intensity <- x[[i]]@intensity[seq(1,length(x[[i]]@intensity),by=by)]
        x[[i]]@mass <- x[[i]]@mass[seq(1,length(x[[i]]@mass),by=by)]
        }
    }
    else {x@intensity <- x@intensity[seq(1,length(x@intensity),by=by)]
          x@mass <- x@mass[seq(1,length(x@mass),by=by)]
         }
    
    return(x)
}