\name{deletePeaks}
\alias{deletePeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Delete peaks from a \code{MassPeaks} objects
}
\description{
This function deletes peaks of height (intensity) below a given value in \code{MassPeaks} objects.
}
\usage{
deletePeaks(x, min = NULL)
}
\arguments{
  \item{x}{
A list of \code{MassPeaks} objects.
}
  \item{min}{
Lower threshold used to discard a peak.
}
}
\details{
This functions takes a list of \code{MassPeaks} objects and filters out peaks of height (intensity) falling below the given minimum value.
}
\value{
A filtered list of \code{MassPeaks} objects.
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

# Delete peaks of intensity < 30

peaks <- deletePeaks(peaks, min = 30)
}
