\name{rawToPeaks}
\alias{rawToPeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create list of \code{\link{MassPeaks}} objects
}
\description{
This is an auxiliary function to create a list of \code{\link{MassPeaks}} objects from raw data.
}
\usage{
rawToPeaks(mz, I)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mz}{
Vector of m/z values.
}
  \item{I}{
Matrix of peak intensity values.
}
}
\details{
This functions creates a list of \code{\link{MassPeaks}} objects from a vector of common m/z values and a matrix of column vectors of peak intensities for a collections of mass peak profiles. The column names are used to label the elements of the list.
}
\value{
A list of \code{\link{MassPeaks}} objects.
}


\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line