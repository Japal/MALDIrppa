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
A list of \code{\link{MassPeaks}} objects.
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
