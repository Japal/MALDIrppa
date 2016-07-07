\name{addMetadata}
\alias{addMetadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add metadata to \code{AbstractMassObject} class objects
}
\description{
This function adds metadata to the \code{metaData} slot of an \code{\link{AbstractMassObject}} class object.
}
\usage{
addMetadata(x, metadata, pos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
List of \code{\link{AbstractMassObject}} (\code{MassSpectra} or \code{MassPeaks}) class objects.
}
  \item{metadata}{
Vector containing the metadata to be included for each element of \code{x} (same length as \code{x}).
}
  \item{pos}{
Position of the new metadata within the \code{metaData} slot list of each element of \code{x}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List of \code{\link{AbstractMassObject}} class objects including the new metadata in their \code{metaData} slot.
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

# Add metadata

info <- paste("Spectrum No.",1:length(spectra)) # Artificial metadata vector
spectra2 <- addMetadata(spectra,info,1)

# Check info in metaData slot

spectra2[[1]]@metaData


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
