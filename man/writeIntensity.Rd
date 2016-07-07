\name{writeIntensity}
\alias{writeIntensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write intensity matrix in different formats
}
\description{
This function writes an intensity matrix in a file in the R, csv or NEXUS format. For NEXUS it allows to specify sample (taxa) labels and peak weights for subsequent analysis.
}
\usage{
writeIntensity(x, file = "intMatrix", format = c("R", "csv", "NEXUS"),
               binary = FALSE, labels = NULL, weights=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Intensity matrix. Rows and columns must represent, respectively, samples and m/z locations.
}
  \item{file}{
A character string specifying a name for the destination file.
}
  \item{format}{
One of R (default \code{.Rdata} file), text (comma-separated \code{.csv} file) or NEXUS (\code{.nex} file).
}
  \item{binary}{
Logical value. If \code{TRUE}, a binary version (1: presence, 0: absence) of \code{x} is saved (default \code{FALSE}). Note that it is internally set to \code{TRUE} when the NEXUS format is chosen.
}
  \item{labels}{
Vector of names for the samples.
}
  \item{weights}{
Numeric vector of weights for the m/z locations (peaks).
}
  \item{\dots}{
Additional arguments for \code{\link{save}} and \code{\link{write.table}}. For NEXUS format options see \code{\link{write.nexus.data}} in the \code{\link{ape}} library (e.g. choices for \code{datablock} or \code{interleaved}).
}
}
\details{
This function is a wrapper to simplify the writing of an intensity matrix (typically produced by \code{\link{intensityMatrix}} in \code{\link{MALDIquant}}) in different formats. It includes the NEXUS format as an extension of the \code{\link{write.nexus.data}} function in the \code{\link{ape}} library to handle peak intensity data. It also allows for taxa/samples names and pre-computed peak weights to be included in the NEXUS file. It checks whether the names meet NEXUS name conventions and format them if not. A binary intensity matrix is always generated when the NEXUS format is chosen. If any, \code{NA} values in \code{x} are assumed to denote zero intensities/peak absence and are then internally converted into zeros.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Load example data

data(spectra) # list of MassSpectra class objects
data(type)    # vector with metadata (spectra IDs)

# Some pre-processing

library(MALDIquant)

spectra <- screenSpectra(spectra)
spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
spectra <- removeBaseline(spectra)
peaks <- detectPeaks(spectra)
peaks <- alignPeaks(peaks)

  # Intensity matrix
int <- intensityMatrix(peaks)

 # default R format
writeIntensity(int,file="int")
 # binary NEXUS file
writeIntensity(int,file="int.binary",format="NEXUS",interleaved=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
