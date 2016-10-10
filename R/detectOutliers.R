detectOutliers <- function(x, by = NULL, binary = FALSE, ...){
  
  if (any(inherits(x, "list") & inherits(x[[1]], "MassPeaks")) == FALSE) {
    stop("x must be a list of MassPeaks class objects")
  }
  if ((!is.null(by)) & (is.vector(by))) {
    if (length(by) != length(x)) {
      stop("The number of elements in x and by do not match")
    }
  }
  if ((!is.null(by)) & (is.data.frame(by) | is.matrix(by))) {
    if (nrow(by) != length(x)) {
      stop("The number of elements in x and by do not match")
    }
  }
  if (binary==TRUE) dist <- "binary"
  
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
  
  TryCatch <- function(expr) {
    # Customised tryCatch to silent warning when error
    warn <- err <- NULL
    value <- withCallingHandlers(
      tryCatch(expr, error=function(e) {
        err <<- e
        NULL
      }), warning=function(w) {
        warn <<- w
        invokeRestart("muffleWarning")
      })
    list(value=value, warning=warn, error=err)
  }
  
  h <- function(w){
    # Handles warning in cmdscale related to non-positive eigenvalues
    # due to non-Euclidean dissimilarity matrix when binary = TRUE
    # (Thanks to Romain Francois)
    if(any(grepl("of the first",w))) invokeRestart("muffleWarning")
  }
  
  out <- function(x,d){
    x.dist <- dist(x,method=d)
    for (i in 1:(floor(nrow(x)/2)-1)){ # search for non-singular cov matrix
      fit <- withCallingHandlers(cmdscale(x.dist,k=floor(nrow(x)/2)-(i-1)),warning=h)
      out <- TryCatch(out <- adapt.out(fit)$outliers)
      if(!is.null(out[[3]])) {next}
      else {break}
    }
    return(out$value)
  } 
  
  int <- intensityMatrix(x)
  if (binary==FALSE) {
    int[is.na(int)==TRUE] <- 0
    dist <- "euclidean"
  }
  if (binary==TRUE) {
    int <- ifelse(is.na(int),0,1)
    dist <- "binary"
  }
  int <- as.data.frame(int)
  
  if (is.null(by)){
    outs <- out(int,dist)
  }
  else{
    by <- droplevels(as.factor(by))
    by <- factor(by,levels=unique(by)) # To keep original order
    outs <- do.call(c,lapply(split(int,by),FUN=out,d=dist))
    outs <- data.frame(Group=by,Atypical=outs)
    rownames(outs) <- NULL
  }
  return(outs)
}