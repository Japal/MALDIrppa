\name{wavSmoothing}
\alias{wavSmoothing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Discrete wavelet transformation for \code{MassSpectrum} objects
}
\description{
This function performs undecimated wavelet transform (UDWT) on mass spectra in \code{\link{MassSpectrum}} format.
}
\usage{
wavSmoothing(x, thresh.scale = 2.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A list of \code{\link{MassSpectrum}} objects.
}
  \item{thresh.scale}{
Smoothing factor.
}
  \item{...}{Other arguments.
}
}
\details{
This is an adaptation of the \code{\link{wavShrink}} function (\code{wmtsa} package) to be used with \code{\link{MassSpectrum}} objects. See \code{wavShrink} in \code{wmtsa} package for options.
}
\value{
A list of \code{\link{MassSpectrum}} objects with denoised signal intensities.
}

\examples{
# Load example data

data(spectra) # list of MassSpectra class objects

# sqrt transformation and signal smoothing using UDWT

spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
