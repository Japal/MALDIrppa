importSpectra <-
function(where=getwd()){
  
  # Import spectra and convert to MassSpectrum (MALDIquant) format
  
  # Requires dat, csv, txt file type formats containing a column with the m/z values and
  # and a column with the intensities (single-space separator)
  
  # where: path to the folder where spectra are stored (default: current working directory)
  
  MSlist <- function(mz,I){
    # mz: common m/z vector
    # I: matrix with spectra intensities by columns (sample IDs included as column names)
    
    d <- list()
    for (i in 1:ncol(I)){
      d[[i]] <- createMassSpectrum(mz,I[,i]) 
    }
    names(d) <- colnames(I)
    d
  }
  
  current <- getwd()
  
  # Read spectra
  setwd(where)
  mz <- read.csv(dir()[1],sep=" ",header=FALSE)[,1]
  intensity <- matrix(0,nrow=length(mz),ncol=length(dir()))
  colnames(intensity) <- sub("^([^.]*).*", "\\1",dir()[1:length(dir())])
  
  for (i in 1:(length(dir()))){
    intensity[,i] <- read.csv(dir()[i],sep=" ",header=FALSE)[,2]
  }
  
  # Convert to MassSpectrum object
  spectra <- MSlist(mz,intensity)
  # Set folder back to current
  setwd(current)
  
  return(spectra) 
}
