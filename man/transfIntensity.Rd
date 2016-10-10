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
  \item{fun}{A user-defined transformation function.
%%     ~~Describe \code{peaks} here~~
}
  \item{...}{Other arguments.
}
}
\details{
Unlike \code{\link{transformIntensity}} in \code{\link{MALDIquant}} which provides a few common transformations, this function allows the user to define any sensible function to be applied on signal intensities. It is used as auxiliary function to other procedures in the package.
}
\value{
A list of \code{\linkS4class{MassSpectrum}} objects with signal intensities transformed according to \code{fun}.
}

\seealso{
\code{\link{transformIntensity}}.
}
\examples{
# Load example data

data(spectra) # list of MassSpectra class objects

# Scale intensities into [0, 1] by dividing by their maximum value

scale.max <- function(x){x/max(x)} # define scaling function

scaled.spectra <- transfIntensity(spectra, fun = scale.max)
    
# Compute natural logarithm of intensity values

log.spectra <- transfIntensity(spectra, fun = function(x) log(x))


}
