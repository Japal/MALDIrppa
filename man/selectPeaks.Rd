\name{selectPeaks}
\alias{selectPeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TBC
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
selectPeaks(peaks, masses)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{peaks}{
%%     ~~Describe \code{peaks} here~~
}
  \item{masses}{
%%     ~~Describe \code{masses} here~~
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
function (peaks, masses) 
{
    peaks2 <- list()
    for (i in 1:length(peaks)) {
        t_int <- intensity(peaks[[i]])
        t_mas <- mass(peaks[[i]])
        mini <- which(round(t_mas, 3) \%in\% masses)
        t_int <- t_int[mini]
        t_mas <- t_mas[mini]
        peaks2[[i]] <- createMassPeaks(mass = t_mas, intensity = t_int)
    }
    peaks2
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
