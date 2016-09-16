\name{summary.scSpectra}
\alias{summary.scSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Numerical summary of results from \code{screenSpectra} function
}
\description{
This function generates a numerical summary of the settings and results from applying the function \code{screenSpectra} to identify potential faulty, low-quality raw mass spectra.
}
\usage{
summary.scSpectra(x, ncases = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{scSpectra} object as generated from \code{\link{screenSpectra}}.
}
  \item{ncases}{
Number of cases shown in the results table.
}
}
\details{
A table is generated that includes details of the numerical estimations along with mass spectra ID, value of the estimated robust scale estimator and the class allocation for each mass spectra, either potentially low-quality (\code{failure}) or good-quality (\code{success}).
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{screenSpectra}} and \code{\link{plot.scSpectra}}.
}
\examples{
# Load example data

data(spectra) # list of MassSpectra objects

scr.spectra <- screenSpectra(spectra)
summary(scr.spectra)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
