importSpectra <- function(where = getwd()){
  
  files <- list.files(path = where, pattern = "\\.csv$|\\.txt$|\\.dat$",ignore.case=TRUE)
  
  # Read spectra
  mz <- read.csv(file.path(where,files[1]), sep = " ", header = FALSE)[,1]
  intensity <- matrix(0, nrow = length(mz), ncol = length(files))
  colnames(intensity) <- sub("^([^.]*).*", "\\1", files[1:length(files)])
  
  for (i in 1:(length(files))){
    intensity[,i] <- read.csv(file.path(where,files[i]), sep = " ", header = FALSE)[,2]
  }
  
  # Convert to MassSpectrum object
  spectra <- rawToSpectra(mz, intensity)
  
  return(spectra) 

}
