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
This functions works with dat, csv or txt file types containing two columns: the first one referring to common m/z values and the second one to intensities (using single-space separator between both and no column names). It reads all the files in the given folder and creates a list of \code{\link{MassSpectrum}} objects. For importing data from more specialised file formats we refer the reader to the package \code{MALDIquantForeign}.
}
\value{
A list of \code{\link{MassSpectrum}} objects.
}

\examples{
# Create fake mass spectrometry data

s1 <- cbind(1:20, rlnorm(20))
s2 <- cbind(1:20, rlnorm(20))
s3 <- cbind(1:20, rlnorm(20))


current <- getwd()
dir.create(paste(getwd(),"/example",sep=""))
setwd(paste(getwd(),"/example",sep=""))

write.table(s1, file="s1.csv", row.names = FALSE, col.names = FALSE, sep=" ")
write.table(s2, file="s2.csv", row.names = FALSE, col.names = FALSE, sep=" ")
write.table(s3, file="s3.csv", row.names = FALSE, col.names = FALSE, sep=" ")

# Import files in ~/example and arrange into a list of MassSpectrum objects

spectra <- importSpectra()

# Delete example folder and back to original working directory
unlink(getwd(),recursive = TRUE)
setwd(current)
}
