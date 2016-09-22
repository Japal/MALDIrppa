\name{detectOutliers}
\alias{detectOutliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Detection of outlying mass peak profiles
}
\description{
This function identifies outlying cases in a collection of processed mass peak profiles. It can be applied either on peak intensities or binary data (peak presence/absence patterns). It allows to specify a grouping factor in order to execute the procedure at the desired level of aggregation.
}
\usage{
detectOutliers(x, by = NULL, binary = FALSE, ...)
}
\arguments{
  \item{x}{A list of \code{\link{MassSpectra}} objects containing processed peaks.}
    \item{by}{If given, a variable (\code{\link{factor}} or \code{\link{numeric}}) defining the grouping elements.}
  \item{binary}{Logical value. It indicates whether the procedure must be applied on a binary version of the data (\code{TRUE}, default) or on peak intensities (\code{FALSE}).}
  \item{...}{Optional arguments for the robust outlier detection method.} 
}
\details{
This function marks samples with mass peak profiles that largely deviates from other samples at the given aggregation level. It uses robust methods for the detection of multivariate outliers applied on metric multidimensional scaling (MDS) coordinates (Euclidean distance is used for peak intensities and binary distance for binary profiles; see \code{\link{dist}}). The number of MDS coordinates used is generally set to p = \code{floor}(n/2), where n is the number of samples in the target subset. This is an upper cap recommended for the computation of the robust MCD estimator by \code{\link{covMcd}}. However, that rule of thumb can still generate matrix singularity problems with \code{\link{covMcd}} in some cases. When this occurs \code{detectOutliers} further reduces p to use the maximum number of MDS coordinates giving rise to a non-singular covariance matrix (min(p) = 2 in any case). This function builds on functions for adaptive multivariate outlier detection implemented in the \code{\link{mvoutlier}} package.
}
\value{
If \code{by = NULL}, a logical vector of length equal to the number of elements of \code{x} indicating outlier samples by \code{TRUE}. Otherwise, a 2-column \code{\link{data.frame}} is generated which includes such a logical vector along with the grouping variable given in \code{by}.
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
data(isolate) # isolate IDs

# Some pre-processing

sc.results <- screenSpectra(spectra,meta=isolate)
spectra <- sc.results$fspectra # Filtered mass spectra
isolate <- sc.results$fmeta    # Filtered metadata  
spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
spectra <- removeBaseline(spectra)
peaks <- detectPeaks(spectra)
peaks <- alignPeaks(peaks, minFreq = 0.5)

# Find outlying samples at isolate level

out <- detectOutliers(peaks, by = isolate)

# From peak presence/absence profiles

out.binary <- detectOutliers(peaks, by = isolate, binary = TRUE)

  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
