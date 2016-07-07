\name{plot.scSpectra}
\alias{plot.scSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TBC
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.scSpectra(x, plot = c("index", "hist"), breaks = 30, col = "green3", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{plot}{
Type of graphical display.
}
  \item{breaks}{
Number of break points for plotting the histogram (default = 30).
}
  \item{col}{
Histogram bar colour.
}
  \item{...}{
Other arguments.
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
function (x, plot = c("index", "hist"), ...) 
{
    if (inherits(x, "mads") == FALSE) {
        stop("x must be a mads class object")
    }
    suppressPackageStartupMessages(library(lattice))
    u <- as.numeric(x$upper)
    l <- as.numeric(x$lower)
    labels <- 1:length(x$mad.table[, 1])
    cols <- ifelse(x$mad.table[, 1] <= l | x$mad.table[, 1] >= 
        u, "red3", "blue3")
    if (plot == "index") {
        xyplot(mad.table[, 1] ~ 1:length(mad.table[, 1]), data = x, 
            cex = 0.75, col = "white", ylab = paste("MAD scaled ", 
                x$nd, "-derivative", sep = ""), xlab = "Index", 
            panel = function(x, y, ...) {
                panel.xyplot(x, y, ...)
                panel.abline(h = u, lty = 3, col = "black")
                panel.abline(h = l, lty = 3, col = "black")
                ltext(x, y, labels = labels, cex = 0.8, col = cols)
            })
    }
    else {
        histogram(x$mad.table[, 1], breaks = 30, xlab = paste("MAD scaled ", 
            x$nd, "-derivative", sep = ""), col = "green3", panel = function(x, 
            ...) {
            panel.histogram(x, ...)
            panel.abline(v = u, lty = 3, col = "black")
            panel.abline(v = l, lty = 3, col = "black")
        })
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
