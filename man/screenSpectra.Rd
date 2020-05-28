\name{screenSpectra}
\alias{screenSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identification of potentially low-quality raw mass spectra
}
\description{
This function implements a quality control check to help in the identification of possibly faulty, low-quality raw mass spectra. It computes an atypicality score and labels suspicious profiles for further inspection and filtering.
}
\usage{
screenSpectra(x, meta = NULL, threshold = 1.5, estimator = c("Q", "MAD"),
                 method = c("adj.boxplot", "boxplot", "ESD", "Hampel", "RC"),
                 nd = 1, lambda = 0.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A list of \code{\link{MassSpectrum}} objects.
}
  \item{meta}{(optional) Matrix or vector containing metadata associated to \code{x}. Typically a data matrix including spectrum ID, biotype, replicate number, etc. for each element of \code{x}.
}
  \item{threshold}{
Multiplicative factor used in computing the upper and lower fences to determine passes and failures. It is related to the actual method used to compute the fences (see \code{method}). Typically, \code{threshold = 1.5} (default value) for the boxplot rules, and \code{threshold = 3} for the others.
}
  \item{estimator}{Robust scale estimator used:
  
  \code{Q}: robust location-free scale estimate (default, see \code{\link{Qn}} function in \code{robustbase} package). More efficient than MAD and adequate for non-symmetric distributions.
  
    \code{MAD}: median absolute deviance scale estimate. Very robust and preferred for fairly symmetric distributions.
}
\item{method}{
Method used to compute upper and lower fences for the identification of atypical mass spectra.

\code{boxplot}: standard boxplot rule based on the first and third quartiles and the interquartile range. 

\code{adj.boxplot}: extension of boxplot rule for strongly asymmetric data (default).

\code{ESD}: extreme studentized deviation method. Based on the mean and the standard deviation of the data. Typically used with \code{threshold = 3} (three-sigma rule).

\code{Hampel}: robust version of the ESD method based on the median and the median absolute deviance estimate (MAD).

\code{RC}: as Hampel's but replacing MAD by Rousseeuw & Croux (1993)'s \code{\link{Qn}} as scale estimate.
}
  \item{nd}{
Order for the derivative function of the mass spectra (default = 1).
}
  \item{lambda}{
Weight given to each component of the atypicality score (values in [0, 1], default = 0.5, see details below).
}
  \item{...}{
Other arguments.
}
}
\details{
The procedure computes an atypicality score (A score) based on a weighted function of two components: (1) a robust scale estimator (\code{Q} or \code{MAD}) of the n-order derivative (computed using Savitzky-Golay smoothing filter) of scaled mass spectra and (2) the median intensity of the signals. Given a \code{method} to determine tolerance fences, a mass spectrum is labelled as potentially faulty, low-quality according to the magnitude of its A score. The \code{adj.boxplot} method based on the \code{Q} scale estimator and equal weights to both components (\code{lambda} = 0.5) are the default options. The greater \code{lambda} the higher the weight given to the scale estimator in the A score. The function produces summaries and a list of mass spectra and (if given) associated metadata in which the identified cases were filtered out.
}
\value{

An object of class \code{scSpectra} with elements:

\item{fspectra }{List of mass spectra (\code{MassSpectrum} class) with potential low-quality cases filtered out.}
\item{fmeta }{Associated filtered metadata (\code{data.frame} object).}
\item{est.table }{Results table showing the mass spectra ID, A score and label (pass/failure).}
\item{... }{Other details (see method \code{\link{summary.scSpectra}} for \code{scSpectra} objects).}
}

\seealso{
See methods \code{\link{summary.scSpectra}} and \code{\link{plot.scSpectra}} for \code{scSpectra} objects.
}
\examples{

# Load example data

data(spectra) # list of MassSpectra objects
data(type)    # metadata

# Results using different settings

sc.results <- screenSpectra(spectra)
sc.results <- screenSpectra(spectra, type)
sc.results <- screenSpectra(spectra, type, method = "RC")
sc.results <- screenSpectra(spectra, type, threshold = 3, estimator = "MAD", method = "Hampel")

# Numerical and graphical summary

summary(sc.results)
plot(sc.results)

# Save filtered data for further pre-processing

filtered.spectra <- sc.results$fspectra
filtered.type <- sc.results$fmeta
}
