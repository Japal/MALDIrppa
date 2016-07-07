estimateNoiseLowess <-
function(x, y, ...) {
  n <- stats::lowess(x=x, y=y, ...);
  return(cbind(mass=x, intensity=n$y));
}
