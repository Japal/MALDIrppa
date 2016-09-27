\name{summarySpectra}
\alias{summarySpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary of mass spectra
}
\description{
This function generates a numerical summary of a collection of \code{\link{MassSpectrum}} objects.
}
\usage{
summarySpectra(x, ncases = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A list of \code{\link{MassSpectrum}} objects.
}
  \item{ncases}{
Number of cases shown in the results table (default is \code{NULL}, all cases).
}
  \item{...}{
Other arguments.
}
}
\details{
For each sample this function provides summary statistics of m/z points and signal intensities (number, minimum, mean, standard deviation, median, mean absolute deviation, maximum).
}
\value{
A \code{\link{data.frame}} containing summary information of a collection of \code{\link{MassSpectrum}} objects.
}

\examples{
# Load example data

data(spectra) # list of MassSpectra class objects
data(type)    # vector with metadata (spectra IDs)

# Summary of spectra features (show first 20 samples)

summarySpectra(spectra, 20)

# Some pre-processing

sc.results <- screenSpectra(spectra, meta = type)

spectra <- sc.results$fspectra
type <- sc.results$fmeta

spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
spectra <- removeBaseline(spectra)

names(spectra) <- type # Sample IDs are lost with removeBaseline()

# Summary of spectra features (show first 20 samples)

summarySpectra(spectra, 20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
