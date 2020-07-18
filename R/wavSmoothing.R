wavSmoothing <- function(x, method=c("Wavelet","SavitzkyGolay","MovingAverage"), thresh.scale = 2.5, ...){
  
  if (!isMassSpectrumList(x)) {
    stop("x must be a list of MassSpectrum class objects")
  }
  
  method <- match.arg(method)
  
  if (method=="SavitzkyGolay") {x <- MALDIquant::smoothIntensity(x, method="SavitzkyGolay", ...)}
  if (method=="MovingAverage") {x <- MALDIquant::smoothIntensity(x, method="MovingAverage", ...)}
  
  ### Functions needed from archived ifultools package ########
  
  checkScalarType <- function(x, isType="numeric"){
    
    if (!is.character(isType))
      stop("isType must be an object of class character")
    
    # In R, class(5) is "numeric", not "integer" so we
    # have to accommodate
    #
    # also, if S-PLUS in R parse mode, then integers are converted to floats (1 to 1.)
    # so we accommodate that as well. (set.parse.mode(-1) == 1) means that S-PLUS is in
    # "R" parse mode
    #if ((is.R() || (!is.R() && as.logical(set.parse.mode(-1)))) && isType == "integer"){
    if (isType == "integer")
    {
      if (!is.numeric(x) || length(x) > 1)
        stop("Input must be scalar of class ", isType)
    }
    else{
      if (!eval(parse(text=paste("is.", isType, "(x)", sep=""))) || length(x) > 1)
        stop(" Input must be scalar of class ", isType)
    }
    
    invisible(NULL)
  }
  
  checkVectorType <- function(x, isType="numeric"){
    checkScalarType(isType,"character")
    
    # In R, class(c(1,3:5) is "numeric", not "integer" so we
    # have to accommodate
    #
    # also, if S-PLUS in R parse mode, then integers are converted to floats (1 to 1.)
    # so we accommodate that as well. (set.parse.mode(-1) == 1) means that S-PLUS is in
    # "R" parse mode
    #if ((is.R() || (!is.R() && as.logical(set.parse.mode(-1)))) && isType == "integer"){
    if (isType == "integer")
    {
      
      if (!isVectorAtomic(x) || !is.numeric(x))
        stop("Input must be a vector of class ", isType)
    }
    else{
      
      if (!isVectorAtomic(x) || !eval(parse(text=paste("is.", isType, "(x)", sep=""))))
        stop("Input must be a vector of class ", isType)
    }
    
    invisible(NULL)
  }
  
  isVectorAtomic <- function(x){
    return(is.atomic(x) & any(c(NROW(x),NCOL(x)) == 1))}
  
  ifelse1 <- function(test, x, y, ...){if(test) x else if(missing(..1)) y else ifelse1(y, ...)}
  
  ilogb <- function (x, base = 2, eps = .Machine$double.eps * 1e+09){ # From ifultools package 
    as.integer(logb(x, base = base) + eps)
  }
  
  wavDaubechies <- function (wavelet = "s8", normalized = TRUE){
    filter <- mutilsFilterType(wavelet = wavelet)
    data <- itCall("RS_wavelets_filters_daubechies", as.integer(filter$length), 
                   as.integer(filter$type), as.logical(normalized))
    family <- switch(filter$type + 1, "Extremal Phase", "Least Asymmetric", 
                     "Best Localized", "Coiflet")
    filters <- list(wavelet = as.vector(data[[1]]), scaling = as.vector(data[[2]]), 
                    family = family, length = filter$length, normalized = normalized, 
                    name = wavelet)
    oldClass(filters) <- "wavDaubechies"
    filters
  }
  
  mutilsFilterType <- function (wavelet = "s8"){
    if (tolower(wavelet) == "haar") {
      filter.length <- 2
      filter.type <- 7
    }
    else {
      supported <- c("d", "s", "l", "c")
      prefix <- substring(wavelet, 1, 1)
      if (!is.element(prefix, supported)) 
        stop("The specified filter is currently unsupported")
      filter.type <- match(prefix, supported) - 1
      filter.length <- as.integer(substring(wavelet, 2))
    }
    return(list(type = filter.type, length = filter.length))
  }
  
  itCall <- function (symbol, ...)
  {
    args <- list(...)
    CLASSES <- as.character(sapply(args, function(x) class(x)[1L]))
    COPY <- rep(FALSE, length(args))
    .Call(symbol, ..., COPY = COPY, CLASSES = CLASSES)
  }
  
  mutilsWSShrinkageFunction <- function(x)
  {
    checkScalarType(x,"character")
    supported <- c("hard","soft","mid")
    x <- match.arg(x, supported)
    index <- as.integer(pmatch(x, supported) - 1)
    list(index=index, shrinkfun=x)
  }
  
  mutilsWSThresholdFunction <- function(x)
  {
    checkScalarType(x,"character")
    supported <- c("universal","minimax","adaptive")
    x <- match.arg(x, supported)
    index <- as.integer(pmatch(x, supported) - 1)
    list(index=index, threshfun=x)
  }
  
  ###############
  
  wavShrink <- function (x, wavelet = "s8", n.level = ilogb(length(x), base = 2), # Adapted from wmtsa package
                         shrink.fun = "hard", thresh.fun = "universal", threshold = NULL, 
                         thresh.scale = 1, xform = "modwt", noise.variance = -1, reflect = TRUE) 
  {
    series.name <- deparse(substitute(x))
    if (is.element(class(x), c("named", "signalSeries"))) 
      x <- as.vector(x)
    if (is.complex(x)) 
      stop("Time series must be real-valued")
    if(!is.numeric(x)) stop("Input must be numeric")
    if (length(x) < 2) 
      stop("Time series must contain more than one point")
    if (any(is.na(x))) 
      stop("Time series contains NA values")
    if(!is.character(xform)) stop("Argument xform must be character")
    if(!is.character(thresh.fun)) stop("Argument thresh.fun must be character")
    if(!is.character(shrink.fun)) stop("Argument shrink.fun must be character")
    if(!is.character(wavelet)) stop("Argument wavelet must be character")
    if(!is.numeric(thresh.scale)) stop("Argument xform must be numeric")
    if (is.null(noise.variance)) 
      noise.variance <- -1
    if(!is.numeric(noise.variance)) stop("Argument noise.variance must be numeric")
    xform <- match.arg(tolower(xform), c("dwt", "modwt"))
    if (xform == "modwt") {
      thresh.fun <- "universal"
      decimated <- FALSE
    }
    else decimated <- TRUE
    if (n.level < 1) 
      stop("Number of wavelet transform decomposition levels must be positive")
    if (n.level > ilogb(length(x), base = 2)) 
      stop("Number of wavelet transform decomposition levels exceeds maximum")
    filters <- wavDaubechies(wavelet = wavelet, normalized = !decimated)[c("wavelet", 
                                                                           "scaling")]
    L <- length(filters$wavelet)
    N <- length(x)
    shrfun <- mutilsWSShrinkageFunction(shrink.fun)
    thrfun <- mutilsWSThresholdFunction(thresh.fun)
    if (thresh.scale <= 0) 
      stop("Threshold scaling factor must be positive")
    if (!is.null(threshold)) {
      checkVectorType(threshold, "numeric")
      if (length(threshold) != n.level) {
        threshold <- ifelse1(xform == "modwt", threshold[1]/2^(seq(0, 
                                                                   n.level - 1)/2), rep(threshold[1], n.level))
      }
      if (any(threshold <= 0)) 
        stop("All thresholds must be positive")
    }
    else {
      threshold <- -1
    }
    if (reflect) {
      Lj <- (2^n.level - 1) * (L - 1) + 1
      ix <- seq(N - 1, max(N - Lj, 1))
      x <- c(x, x[ix])
    }
    z <- as.vector(itCall("RS_wavelets_shrink", as.numeric(x), 
                          filters, as.double(matrix(threshold)), thrfun$index, 
                          as.numeric(thresh.scale), as.numeric(noise.variance), 
                          shrfun$index, as.integer(n.level), as.logical(decimated)))
    if (reflect) 
      z <- z[seq(N)]
    attr(z, "wavelet") <- wavelet
    attr(z, "n.level") <- n.level
    attr(z, "shrink.fun") <- shrfun$shrinkfun
    attr(z, "thresh.fun") <- thrfun$threshfun
    attr(z, "thresh.scale") <- thresh.scale
    attr(z, "transform") <- xform
    attr(z, "noise.variance") <- noise.variance
    attr(z, "reflect") <- reflect
    z
  }
  
  ##############
  
  if (method=="Wavelet"){
    x <- lapply(x, FUN = transfIntensity, fun = function(x) wavShrink(x, thresh.scale = thresh.scale, ...))
  }
  
  return(x)  
  
}

