\name{summaryPeaks}
\alias{summaryPeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TBC
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summaryPeaks(peak)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{peak}{
%%     ~~Describe \code{peak} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (peak) 
{
    class <- sapply(peak, function(x) class(x))
    NmzVal <- sapply(peak, function(x) length(x@mass))
    RmzVal <- sapply(peak, function(x) paste(round(min(x@mass), 
        2), "-", round(max(x@mass), 2), sep = ""))
    RintVal <- sapply(peak, function(x) paste(round(min(x@intensity), 
        2), "-", round(max(x@intensity), 2), sep = ""))
    RsnrVal <- sapply(peak, function(x) paste(round(min(x@snr), 
        2), "-", round(max(x@snr), 2), sep = ""))
    return(data.frame(Class = class, Number_peaks = NmzVal, Range_mz_values = RmzVal, 
        Range_intensity_values = RintVal, Range_snr_values = RsnrVal))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
