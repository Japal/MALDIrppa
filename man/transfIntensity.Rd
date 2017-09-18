\name{transfIntensity}
\alias{transfIntensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform intensity of \code{MassSpectrum} objects
}
\description{
This function applies user-defined transformations on the intensities of \code{\linkS4class{MassSpectrum}} objects.
}
\usage{
transfIntensity(x, fun = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A list of \code{\linkS4class{MassSpectrum}} objects.
%%     ~~Describe \code{peaks} here~~
}
  \item{fun}{Name of an user-defined transformation function or any other pre-defined one in R.
%%     ~~Describe \code{peaks} here~~
}
  \item{...}{Other arguments.
}
}
\details{
This function allows the user to define any sensible function to be applied on signal intensities. For logarithm and square root transformations it is equivalent to \code{\link{transformIntensity}} in the MALDIquant package. 
}
\value{
A list of \code{\linkS4class{MassSpectrum}} objects with signal intensities transformed according to \code{fun}.
}

\examples{
# Load example data

data(spectra) # list of MassSpectra class objects

# Scale intensities into [0, 1] by dividing by their maximum value

scale.max <- function(x){x/max(x)} # define scaling function

scaled.spectra <- transfIntensity(spectra, fun = scale.max)
    
# Compute natural logarithm of intensity values (using the pre-defined sqrt R function)

log.spectra <- transfIntensity(spectra, sqrt)


}
