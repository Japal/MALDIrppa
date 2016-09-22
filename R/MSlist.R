MSlist <- function(mz,I){
	
  d <- list()
  for (i in 1:ncol(I)){
    d[[i]] <- createMassSpectrum(mz,I[,i]) 
  }
  names(d) <- colnames(I)
  d
}
