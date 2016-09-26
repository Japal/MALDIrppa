\name{deletePeaks}
\alias{deletePeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Delete peaks from a \code{MassPeaks} objects
}
\description{
This function deletes peaks of height (intensity) below a given value in a \code{MassPeaks} object.
}
\usage{
deletePeaks(peaks, min = NULL)
}
\arguments{
  \item{peaks}{
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

spectra <- screenSpectra(spectra)$fspectra
spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
spectra <- removeBaseline(spectra)
peaks <- detectPeaks(spectra)

# Delete peaks of intensity < 30

peaks <- deletePeaks(peaks, min = 30)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
