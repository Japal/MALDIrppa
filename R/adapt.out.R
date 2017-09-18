.adapt.out <- function (x, quan = 1/2, alpha = 0.025, ...){
  
  rob <- covMcd(x, alpha = quan)
  xarw <- .arw(x, rob$center, rob$cov, alpha = alpha, ...)
  covr <- rob$cov
  mer <- rob$center
  rd <- sqrt(mahalanobis(x, mer, covr))
  o <- (rd > min(sqrt(xarw$cn), sqrt(qchisq(0.975, dim(x)[2]))))
  l <- list(outliers = o, md = rd)
  return(l)

}