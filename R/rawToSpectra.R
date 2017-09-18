rawToSpectra <- function(mz, I){
	
  d <- vector(mode = "list", length = ncol(I))
  
  for (i in 1:ncol(I)){
    d[[i]] <- createMassSpectrum(mz, I[,i]) 
  }
  
  names(d) <- colnames(I)
  
  return(d)

}
