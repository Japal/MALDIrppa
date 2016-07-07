\name{summarySpectra}
\alias{summarySpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TBC
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summarySpectra(spec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
%%     ~~Describe \code{spec} here~~
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
function (spec) 
{
    class <- sapply(spec, function(x) class(x))
    NmzVal <- sapply(spec, function(x) length(x@mass))
    RmzVal <- sapply(spec, function(x) paste(min(x@mass), "-", 
        max(x@mass), sep = ""))
    RintVal <- sapply(spec, function(x) paste(round(min(x@intensity), 
        2), "-", round(max(x@intensity), 2), sep = ""))
    return(data.frame(Class = class, Number_mz_values = NmzVal, 
        Range_mz_values = RmzVal, Range_intensity_values = RintVal))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
