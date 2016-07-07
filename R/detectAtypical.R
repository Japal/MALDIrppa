detectAtypical <- function(x, by = NULL, binary = TRUE, ...){
  
  # Only works on binary matrices at the moment!
  
  if (!is.matrix(x) && !is.data.frame(x)) stop("x must be matrix or data.frame.")
  if (any(is.na(x))) stop("x contains NAs. Suggestion: they may represent zero intensities.")
  if (!is.null(by)){
    if (length(by)!=nrow(x)) stop("The number of rows in x and by do not agree")
  }
  
  require(robustbase)
  
  adapt.out <- function (x, quan = 1/2, alpha = 0.025, ...)
    # Adaptive multivariate outlier detection: tailored version from mvoutlier package 
  {
    
    arw <- function (x, m0, c0, alpha, pcrit)
    {
      n <- nrow(x)
      p <- ncol(x)
      if (missing(pcrit)) {
        if (p <= 10) 
          pcrit <- (0.24 - 0.003 * p)/sqrt(n)
        if (p > 10) 
          pcrit <- (0.252 - 0.0018 * p)/sqrt(n)
      }
      if (missing(alpha)) 
        delta <- qchisq(0.975, p)
      else delta <- qchisq(1 - alpha, p)
      d2 <- mahalanobis(x, m0, c0)
      d2ord <- sort(d2)
      dif <- pchisq(d2ord, p) - (0.5:n)/n
      i <- (d2ord >= delta) & (dif > 0)
      if (sum(i) == 0) 
        alfan <- 0
      else alfan <- max(dif[i])
      if (alfan < pcrit) 
        alfan <- 0
      if (alfan > 0) 
        cn <- max(d2ord[n - ceiling(n * alfan)], delta)
      else cn <- Inf
      w <- d2 < cn
      if (sum(w) == 0) {
        m <- m0
        c <- c0
      }
      else {
        m <- apply(x[w, ], 2, mean)
        c1 <- as.matrix(x - rep(1, n) %*% t(m))
        c <- (t(c1) %*% diag(w) %*% c1)/sum(w)
      }
      list(m = m, c = c, cn = cn, w = w)
    }
    
    rob <- covMcd(x, alpha = quan)
    xarw <- arw(x, rob$center, rob$cov, alpha = alpha)
    covr <- rob$cov
    mer <- rob$center
    rd <- sqrt(mahalanobis(x, mer, covr))
    o <- (rd > min(sqrt(xarw$cn), sqrt(qchisq(0.975, dim(x)[2]))))
    l <- list(outliers = o, md = rd)
    return(l)
  }
  
  out <- function(x){
    x.dist <- dist(x,method="binary")
    fit <- cmdscale(x.dist)
    out <- adapt.out(fit)$outliers
    return(out)
  } 
  
  x <- as.data.frame(x)
  
  if (is.null(by)){
    outs <- out(x)
  }
  else{
    by <- droplevels(as.factor(by))
    by <- factor(by,levels=unique(by)) # To keep original order
    outs <- do.call(c,lapply(split(x,by),FUN=out))
    outs <- data.frame(Group=by,Atypical=outs)
    rownames(outs) <- NULL
  }
  return(outs)
}