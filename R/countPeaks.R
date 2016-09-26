countPeaks <- function(x){
  
  if (any(inherits(x,"list") & inherits(x[[1]],"MassPeaks"))==FALSE) {
    stop("x must be a list of MassPeaks class objects")
  }
  
  unlist(lapply(x,function(x) length(mass(x))))
}
