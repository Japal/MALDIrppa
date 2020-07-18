\name{addMetadata}
\alias{addMetadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add metadata to \code{AbstractMassObject} class objects
}
\description{
This function adds metadata to the \code{metaData} slot of an \code{\link{AbstractMassObject-class}} class object.
}
\usage{
addMetadata(x, metadata, pos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
List of \code{\link{AbstractMassObject-class}} (\code{MassSpectra} or \code{MassPeaks}) class objects.
}
  \item{metadata}{
Vector containing the metadata to be included for each element of \code{x} (same length as \code{x}).
}
  \item{pos}{
Position of the new metadata within the \code{metaData} slot list of each element of \code{x}.
}
}

\value{
List of \code{\link{AbstractMassObject-class}} class objects including the new metadata in their \code{metaData} slot.
}

\examples{
# Load example data

data(spectra) # list of MassSpectra class objects
data(type)    # metadata

# Add metadata

info <- paste("Spectrum No.",1:length(spectra)) # Artificial metadata vector

spectra2 <- addMetadata(spectra,info,1)

# Check info in metaData slot

spectra2[[1]]@metaData

}
