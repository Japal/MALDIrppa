\name{writeMetadata}
\alias{writeMetadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write metadata in different formats
}
\description{
This function is simply a wrapper to write the metadata associated with a collection of mass spectra into a file in either the R or csv format.
}
\usage{
writeMetadata(x, filename = "Metadata", format = c("R", "csv"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Metadata in any sensible data format, preferably \code{\link{matrix}} or \code{\link{data.frame}}.
}
  \item{filename}{
A character string specifying a name for the destination file (filename extension not required).
}
  \item{format}{
One of R (default \code{.RData} file) or text (comma-separated \code{.csv} file).
}
  \item{\dots}{
Other arguments.
}
}
\details{
It uses either \code{\link{save}} or \code{\link{write.table}} to store the metadata. Check these functions for adequate data formats.
}
\value{No return value, file in selected format created on destination folder.}
\examples{
# Load example data

data(spectra) # list of MassSpectra class objects
data(type)    # metadata

# Some pre-processing

sc.spectra <- screenSpectra(spectra, meta = type)

spectra <- sc.spectra$fspectra # filtered spectra
type <- sc.spectra$fmeta # filtered metadata

spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
spectra <- removeBaseline(spectra)
peaks <- detectPeaks(spectra)
peaks <- alignPeaks(peaks, minFreq = 0.8)

# Intensity matrix

int <- intensityMatrix(peaks)

# Save resulting data in R format (to a temporary location as an example)
\donttest{
writeIntensity(int, filename = file.path(tempdir(),"MyintMatrix"))
writeMetadata(type, filename = file.path(tempdir(),"MyMetadata"))
}
# Save resulting data in csv format (to a temporary location as an example)
\donttest{
writeIntensity(int, filename = file.path(tempdir(),"MyintMatrix"),
               format = "csv")
writeMetadata(type, filename = file.path(tempdir(),"MyMetadata"),
               format = "csv")
}
}
