deletePeaks <- function(x,min=NULL){
  
  if (any(inherits(x,"list") & inherits(x[[1]],"MassPeaks"))==FALSE) {
    stop("x must be a list of MassPeaks class objects")
  }
  if (is.null(min)){
    stop("A minimum peak height value must be given") 
  }
  
  peaks2 <- list()
  
  for (i in 1:length(x)){
    t_int <- intensity(x[[i]])
    t_mas <- mass(x[[i]])
    mini <- which(t_int < min)
    t_int <- t_int[-mini]
    t_mas <- t_mas[-mini]
    peaks2[[i]] <- createMassPeaks(mass=t_mas,intensity=t_int)
  }
  peaks2
}
