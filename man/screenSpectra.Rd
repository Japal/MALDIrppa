\name{screenSpectra}
\alias{screenSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identification of potentially low-quality raw mass spectra
}
\description{
This function implements a quality control check to help in the identification of possibly faulty, low-quality raw mass spectra. It labels suspicious profiles for further inspection. It requires a list of \code{\link{MassSpectrum}} objects as input (see \code{\link{MALDIquant}} package).
}
\usage{
screenSpectra(x, meta = NULL, threshold = 1.5, estimator = c("Q", "MAD"),
                 method = c("adj.boxplot", "boxplot", "ESD", "Hampel", "RC"),
                 nd = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Raw mass spectra arranged as a list of \code{\link{MassSpectra}} objects.
}
  \item{meta}{(optional) Vector or
matrix containing metadata associated to the mass spectra in x. Typically a number of columns including spectrum ID, biotype, replicate number, etc. The length of the vector or the number of rows of the data matrix must match the number of elements in \code{x}.
}
  \item{threshold}{
Multiplicative factor used in computing the upper and lower fences to determine passes and failures. It is related to the actual method used to compute the fences (see \code{method}). Typically, \code{threshold = 1.5} (default value) for the boxplot rules, and \code{threshold = 3} for the others.
}
  \item{estimator}{Robust scale estimator used:
  
  \code{MAD}: median absolute deviance scale estimate. Very robust and preferred for fairly symmetric distributions.
  
  \code{Q}: robust location-free scale estimate (see \code{\link{Qn}} function in \code{\link{robustbase}}, default). More efficient than MAD and adequate for non-symmetric distributions.
  
}
\item{method}{
Method used to compute upper and lower fences for the identification of anomalous mass spectra.

\code{boxplot}: standard boxplot rule based on the first and third quartiles and the interquartile range. 

\code{adj.boxplot}: extension of boxplot rule for strongly asymmetric data (default).

\code{ESD}: extreme studentized deviation method. Based on the mean and the standard deviation of the data. Typically used with \code{threshold = 3} (three-sigma rule).

\code{Hampel}: robust version of the ESD method based on the median and the median absolute deviance estimate (MAD).

\code{RC}: as Hampel's but replacing MAD by Rousseeuw & Croux (1993)'s \code{\link{Qn}} as scale estimate.
}
  \item{nd}{
Order for the derivative function of the mass spectra (default = 1).
}
  \item{...}{
Other arguments.
}
}
\details{
The procedure is based on the analysis of the variability (\code{Q} or \code{MAD} scale estimator) of the n-order derivative (computed using Savitzky-Golay smoothing filter) of scaled mass spectra. Extreme values, according to the selected \code{method}, are labelled as potential failures. The default is \code{adj.boxplot} method on \code{Q} scale estimator of the first-order derivative. The function produces summaries and a list of mass spectra and (if given) associated metadata in which the identified cases were filtered out to be used in subsequent pre-processing stages.
}
\value{

An object of class \code{scSpectra} with elements:

\item{fspectra }{List of mass spectra (\code{MassSpectra} class) with potential low-quality cases filtered out.}
\item{fmeta }{Associated filtered metadata (\code{data.frame} object).}
\item{est.table }{Results table showing the mass spectra ID, estimated value of the estimator used and allocated class (pass/failure).}
\item{... }{Other details (see method \code{\link{summary.scSpectra}} for \code{scSpectra} objects).}
}
\references{
%%
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See methods \code{\link{summary.scSpectra}} and \code{\link{plot.scSpectra}} for \code{scSpectra} objects.
}
\examples{

# Load example data

data(spectra) # list of MassSpectra objects
data(type)    # vector of metadata (mass spectra IDs)

# Results using different settings

scr.spectra <- screenSpectra(spectra)
scr.spectra <- screenSpectra(spectra,type)
scr.spectra <- screenSpectra(spectra,type,method="RC")
scr.spectra <- screenSpectra(spectra,type,threshold=3,estimator="MAD",method="Hampel")

# Numerical and graphical summary

summary(scr.spectra)
plot(scr.spectra)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
