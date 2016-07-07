\name{detectAtypical}
\alias{detectAtypical}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Detection of atypical peak intensity profiles
}
\description{
This function identifies atypical (extreme, outlying) samples in a collection of pre-processed peak intensity profiles. It allows to specify a grouping variable in order to apply the procedure to different subsets of samples.
}
\usage{
detectAtypical(x, by = NULL, binary = TRUE, ...)
}
\arguments{
  \item{x}{\code{\link{matrix}} or \code{\link{data.frame}} containing a list of intensities for each sample. Rows and columns must represent, respectively, samples and m/z locations.}
    \item{by}{If given, a variable (\code{\link{factor}} or \code{\link{numeric}}) defining the grouping elements.}
  \item{binary}{Logical value. Indicates whether the intensity matrix \code{x} is given in binary (\code{TRUE}) or numerical format (\code{FALSE}).
  Note: only works on binary data at the moment!}
  \item{...}{Optional arguments for the robust outlier detection method.} 
}
\details{
The procedure labels samples with atypical peak intensity patterns which largely deviates from other samples of the same kind. It is based on robust methods for the detection of multivariate outliers applied on the coordinates from a metric multidimensional scaling (MDS) representation of the pairwise distances between samples.
}
\value{
If \code{by = NULL}, a logical vector of length equal to the number of rows of \code{x} indicating atypical samples. Otherwise, a 2-column \code{\link{data.frame}} including such a logical vector along with the grouping variable in \code{by}. 
%%\item{outlier }{}
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

  # Find atypical samples from data in binary format

int <- intensityMatrix(peaks)
int.binary <- ifelse(is.na(int),0,1)
result <- detectAtypical(int.binary)
  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
