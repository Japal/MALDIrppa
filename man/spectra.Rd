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
Note that these data are simply a random subset of a larger real data set. They are only provided for illustration purposes and results from using them cannot be interpreted in any sensible way.
}

\examples{
data(spectra)

str(spectra[[1]])
plot(spectra[[1]])
}
\keyword{datasets}
