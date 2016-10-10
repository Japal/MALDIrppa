\name{snrPeaks}
\alias{snrPeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract signal-to-noise ratio thresholds from \code{MassPeaks} objects
}
\description{
This function extracts the thresholds used to determine peaks from mass spectra based on signal-to-noise ratio (SNR) (threshold equal to \code{SNR*noise}).
}
\usage{
snrPeaks(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A list of \code{\link{MassPeaks}} objects.
}
}
\details{
Given a collection of \code{\link{MassPeaks}} objects as obtained from \code{\link{detectPeaks}}, this function provides the thresholds used in each case to determine peaks from the original mass spectra. The thresholds are calculated as the product of a SNR value set by the user and the estimated noise of the signal (see \code{\link{detectPeaks}}).
}
\value{
A list of vectors of SNR-based thresholds, one for each sample.
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

# Extract thresholds for each mass peak profile

SNRs <- snrPeaks(peaks)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
