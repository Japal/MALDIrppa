MSlist2 <-
function(mz,I){
  # Generate list of MassPeaks objects for MALDIquant (sampleIDs as list element names)
  # mz: common m/z vector
  # I: matrix with spectra intensities by columns (sample IDs included as column names)
   
  d <- list()
  for (i in 1:ncol(I)){
    d[[i]] <- createMassPeaks(mz,I[,i]) 
  }
  names(d) <- colnames(I)
  d
}
