peakPatterns <- function(x, abs.lab = NA, barplot = TRUE,
                          axis.lab = c("m/z", "Index"),
                          bar.col = "red3",
                          cell.col = c("white", "dodgerblue"),
                          grid = FALSE, grid.col = "black", grid.lty = "dotted",
                          cex.axis = 0.5, cex.lab = 0.5, ...){
  

  if ((!is.matrix(x)) && (!is.data.frame(x)) && (!is.list(x))) stop("x is not a valid object")
  
  if (is.null(abs.lab)) stop("A value for abs.lab must be given")
  
  if (!is.na(abs.lab)){
    if (!any(x==abs.lab,na.rm=TRUE)) stop(paste("Label",abs.lab,"was not found in the data set"))
  }

  if (isMassPeaksList(x)) {
    x <- intensityMatrix(x)
  }
  
  x <- as.data.frame(x)
  
  n <- nrow(x); p <- ncol(x)
  x[x==abs.lab] <- NA
  
  np <- as.data.frame(is.na(x)*1)

  prop.col <- 100-round(colSums(np)/n*100,2)
  
  .plot.patterns(a = np, x = x, barplot = barplot, axis.lab = axis.lab,
                 bar.col = bar.col, grid = grid,
                 cell.col = cell.col, cex.axis = cex.axis,
                 cex.lab = cex.lab, prop.col = prop.col, 
                 grid.col = grid.col, grid.lty = grid.lty, ...)

}