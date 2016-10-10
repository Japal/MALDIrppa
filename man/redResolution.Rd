\name{redResolution}
\alias{redResolution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reduce resolution of \code{MassSpectrum} objects
}
\description{
This function allows to obtain a lighter version of a list of \code{\linkS4class{MassSpectrum}} objects by decreasing their m/z resolution.
}
\usage{
redResolution(x, by = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A list of \code{\linkS4class{MassSpectrum}} objects.
%%     ~~Describe \code{peaks} here~~
}
  \item{by}{Number of times reduction (\code{by = 1}, default).
%%     ~~Describe \code{peaks} here~~
}
}
\details{
This function reduces the resolution of mass spectra by eliminating a regular sequence of m/z sampling points in steps given by the argument \code{by}. For example, specifiying \code{by = 2} means to reduce the length and memory usage of the signal by a half approximately.
}
\value{
A list of \code{\linkS4class{MassSpectrum}} objects.
}

\examples{
# Load example data

data(spectra) # list of MassSpectra class objects

# Reduce resolution by a half

spectra.LowRes <- redResolution(spectra, by = 2)

}
