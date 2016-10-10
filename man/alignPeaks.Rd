\name{alignPeaks}
\alias{alignPeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compact peak alignment process for \code{MassPeaks} objects
}
\description{
This function provides a single command for selecting anchor peaks, peak alignment and binning of \code{\link{MassPeaks}} class objects (\code{\link{MALDIquant}} package). It also deals with alignment-related issues found in high-resolution mass spectrometry data.
}
\usage{
alignPeaks(x, minFreq = 0.9, tolerance = 0.003, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
List of \code{\link{MassPeaks}} class objects containing extracted peaks.
}
  \item{minFreq}{
Minimum relative frequency of a peak over \code{x} to be considered as anchor peak for alignment (see \code{\link{referencePeaks}}).
}
  \item{tolerance}{
Maximal deviation in peak masses to be considered as identical (see \code{\link{referencePeaks}}, \code{\link{binPeaks}}).
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
See \code{\link{warpMassPeaks}} and \code{\link{binPeaks}} in the \code{\link{MALDIquant}} package for details about the alignment and binning algorithms. Note that \code{alignPeaks} applies an additional binning round which helps to correct for misalignment issues found after using the default \code{strict} or \code{relaxed} bin creation rules in high-resolution mass spectrometry data.
}
\value{
A list of \code{\link{MassPeaks}} class objects with aligned peaks along a common m/z range. 
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

# Peak alignment

peaks <- alignPeaks(peaks, minFreq = 0.8)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
