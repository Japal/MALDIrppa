\name{summary.scSpectra}
\alias{summary.scSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TBC
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.scSpectra(x, ncases = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{ncases}{
%%     ~~Describe \code{ncases} here~~
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
function (x, ncases = 10) 
{
    if (inherits(x, "mads") == FALSE) {
        stop("x must be a mads class object")
    }
    cat(paste("MAD scaled ", x$nd, "-derivative:\n (", ncases, 
        " first spectra) \n", sep = ""))
    print(head(x$mad.table, ncases))
    cat("\n")
    cat("----------------------------")
    cat("\n\n")
    cat(paste("Method:", x$met, "\n"))
    cat(paste("Threshold:", x$threshold, "\n"))
    cat(paste("Limits: [", round(x$lower, 4), ",", round(x$upper, 
        4), "] \n", sep = ""))
    cat(paste("Deriv. order:", x$nd, "\n"))
    cat(paste("Runs:", x$runs, "\n"))
    cat(paste("Number of flats:", x$cflat, "\n"))
    cat(paste("Number of potential hairy:", x$chairy, "\n"))
    cat(paste("Number of potential ion-suppression:", x$cions, 
        "\n"))
    cat(paste("Overall percentage cases:", x$prop * 100, "\%"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
