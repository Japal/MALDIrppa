deletePeaks <-
function(peaks,limit){
  
  peaks2 <- list()
  
  for (i in 1:length(peaks)){
    t_int <- intensity(peaks[[i]])
    t_mas <- mass(peaks[[i]])
    mini <- which(t_int < limit)
    t_int <- t_int[-mini]
    t_mas <- t_mas[-mini]
    peaks2[[i]] <- createMassPeaks(mass=t_mas,intensity=t_int)
  }
  peaks2
}
