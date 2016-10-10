\name{countPeaks}
\alias{countPeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count the number of peaks in \code{MassPeaks} objects
}
\description{
This function provides the number of peaks of each element of a list of \code{\link{MassPeaks}} objects. 
}
\usage{
countPeaks(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
List of \code{\link{MassPeaks}} objects.
}
}

\value{
A vector consisting of the number of peaks for each peak profile in \code{x}.
}

\examples{
# Load example data

data(spectra) # list of MassSpectra class objects

# Some pre-processing

spectra <- screenSpectra(spectra)$fspectra
spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
spectra <- removeBaseline(spectra)
peaks <- detectPeaks(spectra)

# Count peaks

npeaks <- countPeaks(peaks)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
