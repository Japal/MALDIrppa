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
See \code{\link{warpMassPeaks}} and \code{\link{binPeaks}} in the \code{\link{MALDIquant}} package for details about the alignment and binning algorithms. Note that \code{alignPeaks} applies an additional binning round which correct for misalignment issues found after using the default \code{strict} or \code{relaxed} bin creation rules in high-resolution mass spectrometry data.
}
\value{
A list of \code{\link{MassPeaks}} class objects with aligned masses. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Load example data

data(spectra) # list of MassSpectra class objects
data(type)    # vector with metadata (spectra IDs)

# Some pre-processing

library(MALDIquant)

spectra <- screenSpectra(spectra)
spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
spectra <- removeBaseline(spectra)
peaks <- detectPeaks(spectra)

# Peak alignment

peaks <- alignPeaks(peaks)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
