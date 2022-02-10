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
             n.levels = 4, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A list of \code{\link{MassSpectrum}} objects.
}
  \item{method}{
Smoothing method used.
}
  \item{n.levels}{
Depth of the decomposition for wavelet-based smoothing.
}
  \item{...}{Other arguments.
}
}
\details{
Note that from version 1.1.0 of \code{MALDIrppa} wavelet smoothing is conducted by maximal overlap discrete wavelet transformation and universal thresholding of coefficients based on methods available on the \code{waveslim} package. The optimal level of smoothing is determined by model-driven estimates of the thresholds. The parameter \code{n.levels} (values > 0 and <= log(length(x),2)) can be used to tweak the levels to obtain a smoother or rougher result.

Alternatively, smoothing methods \code{SavitzkyGolay} and \code{MovingAverage} from the \code{MALDIquant} package can be called directly from this function.

If the previous implementation of the wavelet method is required please download and install manually source files of version 1.0.5-1 from the archive of old sources of the package (\url{https://CRAN.R-project.org/package=MALDIrppa}).
}
\value{
A list of \code{\link{MassSpectrum}} objects with denoised signal intensities.
}

\examples{
# Load example data

data(spectra) # list of MassSpectra class objects

# sqrt transformation and signal smoothing using UDWT

spectra <- transfIntensity(spectra, fun = "sqrt")
spectra <- wavSmoothing(spectra)
}
