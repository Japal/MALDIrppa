\name{type}
\alias{type}
\docType{data}
\title{
Example mass spectra metadata
}
\description{
Metadata associated to the \code{\link{spectra}} data set containing information about isolate, biological and technical replicate numbers and mass spectra IDs.
}
\usage{data(type)}
\format{
  The format is:
  
 Isolate: Factor w/ 14 levels "280","43","45",..: 2 2 2 2 2 2 2 2 2 2 ...
 
 BioRep : int  1 1 1 1 2 2 2 2 3 3 ...
 
 TechRep: int  1 2 3 4 1 2 3 4 1 2 ...
 
 SpectID: Factor w/ 315 levels "160408C13","160408C14",..: 1 2 3 4 5 6 7 8 9 10 ...
}

\examples{
data(type)
str(type)
}
\keyword{datasets}
