\name{peakPatterns}
\alias{peakPatterns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display peak presence/absence patterns
}
\description{
This function displays the patterns of peak presence and absence in an intensity matrix as generated from \code{\link{intensityMatrix}}.
}
\usage{
peakPatterns(x, abs.lab = NA, barplot = TRUE,
             axis.lab = c("m/z", "Index"), bar.color = "red3",
             cell.colors = c("white", "dodgerblue"), grid = FALSE,
             grid.color = "black", grid.lty = "dotted", cex.axis = 0.5,
             cex.lab = 0.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{\link{matrix}} or \code{\link{data.frame}} object.
}
  \item{abs.lab}{
Unique label used to denote peak absence in \code{x} (\code{NA}, default).
}
  \item{barplot}{
Logical value indicating whether a barplot of relative peak frequency across samples is displayed (\code{TRUE}, default).
}
  \item{axis.lab}{
Vector of axis labels in the \code{c("x", "y")} format.
}
  \item{bar.colors}{
Colour of the bars in the barplot.
}
  \item{cell.colors}{
Vector of colours for the table cells (format \code{c("col.absence","col.presence")}).
}
  \item{grid}{
Logical value indicating whether gridlines are added (\code{FALSE}, default).
}
  \item{grid.color}{
Colour of the gridlines (\code{"black"}, default).
}
  \item{grid.lty}{
Style of the gridlines (\code{"dotted"}, default. See \code{lty} in \code{\link{par}}).
}
  \item{cex.axis}{
Axis tick labels scaling factor relative to default.
}
  \item{cex.lab}{
Axis labels scaling factor relative to default.
}
  \item{\dots}{
Other arguments.
}
}
\details{
The peak presence/absence patterns are displayed by rows from the first (top) to the last (bottom) sample in the data set \code{x} over the range of common m/z points. Positive peaks are by default represented by coloured cells whereas zero or absent peaks are left blank. A barplot on the top margins shows the relative frequency of a peak at each m/z point across samples.
}
\seealso{
See \code{\link{intensityMatrix}}.
}
\examples{
# Load example data

data(spectra) # list of MassSpectra class objects
data(isolate) # isolate IDs

# Some pre-processing

sc.results <- screenSpectra(spectra,meta=isolate)
spectra <- sc.results$fspectra # Filtered mass spectra
isolate <- sc.results$fmeta    # Filtered metadata  
spectra <- transformIntensity(spectra, method = "sqrt")
spectra <- wavSmoothing(spectra)
spectra <- removeBaseline(spectra)
peaks <- detectPeaks(spectra)
peaks <- alignPeaks(peaks, minFreq = 0.5)

int <- intensityMatrix(peaks)

# Display patterns across all data

peakPatterns(int) # shows possible misaligment of peaks in some batches

# Check results within isolate 52

peakPatterns(int[isolate=="52",])

COGER UN SUBCONJUNTO SENSIBLE DE LOS DATOS ORIGINALES 

}

