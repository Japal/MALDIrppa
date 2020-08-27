\name{spectra}
\alias{spectra}
\docType{data}
\title{
Example mass spectra profiles
}
\description{
List of mass spectra (\code{\link{MassSpectrum}} class).  
}
\usage{data(spectra)}

\details{
Low-resolution version of a MALDI-TOF mass spectrometry data set in the range [2500, 13000] m/z provided for illustration purposes. It consists of 4 technical replicates of 5 biological replicates from 19 bacterial isolates (see \code{\link{type}} for associated metadata).
}

\examples{
data(spectra)

str(spectra[[1]])
\donttest{
plot(spectra[[1]])
}
}
\keyword{datasets}
