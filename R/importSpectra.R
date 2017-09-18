importSpectra <- function(where = getwd()){
  
  current <- getwd()
  
  # Read spectra
  setwd(where)
  mz <- read.csv(dir()[1], sep = " ", header = FALSE)[,1]
  intensity <- matrix(0, nrow = length(mz), ncol = length(dir()))
  colnames(intensity) <- sub("^([^.]*).*", "\\1", dir()[1:length(dir())])
  
  for (i in 1:(length(dir()))){
    intensity[,i] <- read.csv(dir()[i], sep = " ", header = FALSE)[,2]
  }
  
  # Convert to MassSpectrum object
  spectra <- rawToSpectra(mz, intensity)
  # Set folder back to current
  setwd(current)
  
  return(spectra) 

}
