\name{summaryPeaks}
\alias{summaryPeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary of mass peak profiles
}
\description{
This function generates a numerical summary of a collection of \code{\link{MassPeaks}} objects.
}
\usage{
summaryPeaks(x, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A list of \code{\link{MassPeaks}} objects.
}
  \item{digits}{
Integer indicating the number of decimal places to be used.
}}
\details{
For each \code{\link{MassPeaks}} on the list this function provides summary statistics of m/z points, peak intensities and SNR thresholds (number, minimum, mean, standard deviation, median, mean absolute deviation, maximum).
}
\value{
A \code{\link{data.frame}} containing summary information of a collection of \code{\link{MassPeaks}} objects.
}

\examples{
# Load example data

data(spectra) # list of MassSpectra class objects
data(type)    # metadata

# Some pre-processing

sc.results <- screenSpectra(spectra, meta = type)

spectra <- sc.results$fspectra
type <- sc.results$fmeta

spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
spectra <- removeBaseline(spectra)
peaks <- detectPeaks(spectra)

names(peaks) <- type$SpectID # spectra IDs are lost with removeBaseline()

# Summary of peak profile features (results for positions 10 to 20)

summaryPeaks(peaks[10:20])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
