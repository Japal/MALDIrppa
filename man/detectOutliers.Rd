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
  \item{x}{A list of \code{\link{MassSpectrum}} objects containing processed peaks.}
    \item{by}{If given, a grouping variable (\code{\link{factor}} or \code{\link{numeric}}) subsetting the data.}
  \item{binary}{Logical value. It indicates whether the procedure must be applied on either peak intensities (\code{FALSE}, default) or on binary peak presence/absence patterns (\code{TRUE}).}
  \item{...}{Optional arguments for the robust outlier detection method.} 
}
\details{
This function marks samples with mass peak profiles that largely deviates from other samples at the given aggregation level. It uses robust methods for the detection of multivariate outliers applied on metric multidimensional scaling (MDS) coordinates (Euclidean distance is used for peak intensities and binary distance for binary profiles; see \code{\link{dist}}). The number of MDS coordinates used is generally set to p = \code{floor}(n/2), where n is the number of samples in the target subset. This is an upper cap recommended for the computation of the robust MCD estimator by \code{\link{covMcd}}. However, that rule of thumb can still generate matrix singularity problems with \code{\link{covMcd}} in some cases. When this occurs \code{detectOutliers} further reduces p to use the maximum number of MDS coordinates giving rise to a non-singular covariance matrix (min(p) = 2 in any case). The adaptive multivariate outlier detection algorithm was adapted from the \code{mvoutlier} package.
}
\value{
If \code{by = NULL}, a logical vector of length equal to the number of elements of \code{x} indicating outlying samples by \code{TRUE}. Otherwise, a 2-column \code{\link{data.frame}} is generated which includes such a logical vector along with the grouping variable given in \code{by}.
%%\item{outlier }{}
}

\examples{
# Load example data

data(spectra) # list of MassSpectra class objects
data(type) # metadata

# Some pre-processing

sc.results <- screenSpectra(spectra,meta=type)
spectra <- sc.results$fspectra # filtered mass spectra
type <- sc.results$fmeta    # filtered metadata  
spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
spectra <- removeBaseline(spectra)
peaks <- detectPeaks(spectra)
peaks <- alignPeaks(peaks, minFreq = 0.8)

# Find outlying samples at isolate level

\dontrun{
out <- detectOutliers(peaks, by = type$isolate)

# From peak presence/absence patterns

out.binary <- detectOutliers(peaks, by = type$isolate, binary = TRUE)
}
  }
