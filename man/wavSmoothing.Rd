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
The \code{Wavelet} method relies on the \code{wavShrink} function of the \code{wmtsa} package and its dependencies (now archived by CRAN). The original C code by William Constantine and Keith L. Davidson, in turn including copyrighted routines by Insightful Corp., has been revised and included into \code{MALDIrppa} for the method to work. Alternatively, smoothing methods \code{SavitzkyGolay} and \code{MovingAverage} from the \code{MALDIquant} package can be called directly from the function.
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
