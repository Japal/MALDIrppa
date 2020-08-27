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
This functions works with dat, csv or txt file types containing two columns: the first one referring to common m/z values and the second one to intensities (using single-space separator between both and no column names). It reads all the .dat, .csv or .txt files in the given folder (so unrelated files should better not be there) and creates a list of \code{\link{MassSpectrum}} objects. For importing data from more specialised file formats we refer the reader to the package \code{MALDIquantForeign}.
}
\value{
A list of \code{\link{MassSpectrum}} objects.
}

\examples{
# Create fake mass spectrometry data
\donttest{
s1 <- cbind(1:20, rlnorm(20))
s2 <- cbind(1:20, rlnorm(20))
s3 <- cbind(1:20, rlnorm(20))

# Save as csv files in temporary directory

path <- tempdir()

write.table(s1, file = file.path(path, "s1.csv"),
            row.names = FALSE, col.names = FALSE, sep=" ")
write.table(s2, file = file.path(path, "s2.csv"),
            row.names = FALSE, col.names = FALSE, sep=" ")
write.table(s3, file = file.path(path, "s3.csv"),
            row.names = FALSE, col.names = FALSE, sep=" ")

# Import files and arrange into a list of MassSpectrum objects

spectra <- importSpectra(where = path)
}
}
