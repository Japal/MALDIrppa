estimateNoiseLoess <-
function(x, y, ...) {
  n <- stats::loess(y ~ x, ...)
  return(cbind(mass=x, intensity=n$fitted))
}
