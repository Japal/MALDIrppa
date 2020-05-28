\name{summary.scSpectra}
\alias{summary.scSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Numerical summary of \code{screenSpectra} objects
}
\description{
This is a \code{summary} method for \code{scSpectra} objects that generates a numerical summary of the settings and results from applying \code{screenSpectra} to identify potential faulty, low-quality raw mass spectra.
}
\usage{
\method{summary}{scSpectra}(object, ncases = 10, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A \code{scSpectra} object as generated from \code{\link{screenSpectra}}.
}
  \item{ncases}{
Number of cases shown in the results table.
}
  \item{...}{
Other arguments.
}
}
\details{
A table is generated that includes details of the numerical estimations along with mass spectra ID, A score and the label for each mass spectra, either potentially low-quality (\code{failure}) or good-quality (\code{success}).
}
\seealso{
See \code{\link{screenSpectra}} and \code{\link{plot.scSpectra}}.
}
\examples{
# Load example data

data(spectra) # list of MassSpectra objects

sc.results <- screenSpectra(spectra)
summary(sc.results)
}
