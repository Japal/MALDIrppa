importSpectra <- function(where=getwd()){

  MSlist <- function(mz,I){
    
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
