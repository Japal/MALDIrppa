\name{wavSmoothing}
\alias{wavSmoothing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Discrete wavelet transformation for \code{MassSpectrum} objects
}
\description{
This function performs undecimated wavelet transform (UDWT) on mass spectra in \code{\link{MassSpectrum}} format. Alternatively, smoothing methods included in the \code{MALDIquant} package can be called.
}
\usage{
wavSmoothing(x, method = c("Wavelet", "SavitzkyGolay", "MovingAverage"),
             thresh.scale = 2.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A list of \code{\link{MassSpectrum}} objects.
}
  \item{method}{
Smoothing method used.
}
  \item{thresh.scale}{
Smoothing factor for wavelet-based smoothing.
}
  \item{...}{Other arguments.
}
}
\details{
This provides an adaptation of the \code{wavShrink} function (\code{wmtsa} package) to be used with \code{\link{MassSpectrum}} objects. Alternatively, the function can be used to call the smoothing methods included with the \code{smoothIntensity} function in the \code{MALDIquant} package. Check the documentation of \code{wavShrink} or \code{smoothIntensity} for addtional options.
}
\value{
A list of \code{\link{MassSpectrum}} objects with denoised signal intensities.
}

\examples{
# Load example data

data(spectra) # list of MassSpectra class objects

# sqrt transformation and signal smoothing using UDWT

spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra, method = "SavitzkyGolay")
}
