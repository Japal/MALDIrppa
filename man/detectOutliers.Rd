\name{detectOutliers}
\alias{detectOutliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Detection of outlier peak profiles
}
\description{
This function identifies outlying samples in a collection of processed peak profiles. It can be applied directly on peak intensities or on binary data (peak presence/abscense). It allows to specify a grouping factor in order to execute the procedure at the desired level of aggregation.
}
\usage{
detectOutliers(x, by = NULL, binary = TRUE, ...)
}
\arguments{
  \item{x}{A list of \code{\link{MassSpectra}} objects containing processed peaks.}
    \item{by}{If given, a variable (\code{\link{factor}} or \code{\link{numeric}}) defining the grouping elements.}
  \item{binary}{Logical value. It indicates whether the procedure must be applied on a binary version of the data or on intensities.}
  \item{...}{Optional arguments for the robust outlier detection method.} 
}
\details{
This function marks samples with peak profiles that largely deviates from other samples of the same kind. It uses robust methods for the detection of multivariate outliers applied on multidimensional scaling (MDS) coordinates. The number of MDS coordinates used is set to p = n/2, where n is the number of spectra in the target subset. This is an upper cap required for the computation of the robust MCD estimator. This function builds on functions for the adaptive multivariate outlier detection implemented in the \code{\link{mvoutlier}} package.
}
\value{
If \code{by = NULL}, a logical vector of length equal to the number of elements of \code{x} indicating outlier samples. Otherwise, a 2-column \code{\link{data.frame}} including such a logical vector along with the grouping variable in \code{by}.
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

 out.binary <- detectOutliers(peaks, by = type$Isolate, binary = TRUE)
  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
