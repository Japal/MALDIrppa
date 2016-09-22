\name{importSpectra}
\alias{importSpectra}
\title{
Convert mass spectra from text files into \code{MassSpectrum} objects
}
\description{
This function allows to import collections of mass spectra stored in individual text files into a list of \code{\link{MassSpectrum}} objects.
}
\usage{
importSpectra(where = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{where}{
Path to the folder where the text files are stored (default: current working directory).
}
}
\details{
This functions works with dat, csv or txt file types containing two columns: the first one refering to common m/z values and the second one to intensities (using single-space separator between both and no column names). It reads all the files in the given folder and creates a list of \code{\link{MassSpectrum}} objects.
}
\value{
A list of \code{\link{MassSpectrum}} objects.
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (where = getwd()) 
{
    MSlist <- function(mz, I) {
        require(MALDIquant)
        d <- list()
        for (i in 1:ncol(I)) {
            d[[i]] <- createMassSpectrum(mz, I[, i])
        }
        names(d) <- colnames(I)
        d
    }
    current <- getwd()
    setwd(where)
    mz <- read.csv(dir()[1], sep = " ", header = FALSE)[, 1]
    intensity <- matrix(0, nrow = length(mz), ncol = length(dir()))
    colnames(intensity) <- sub("^([^.]*).*", "\\1", dir()[1:length(dir())])
    for (i in 1:(length(dir()))) {
        intensity[, i] <- read.csv(dir()[i], sep = " ", header = FALSE)[, 
            2]
    }
    spectra <- MSlist(mz, intensity)
    setwd(current)
    return(spectra)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
