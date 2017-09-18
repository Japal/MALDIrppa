detectOutliers <- function(x, by = NULL, binary = FALSE, ...){

  if (!isMassPeaksList(x)) {
    stop("x must be a list of MassPeaks class objects")
  }
  
  if ((!is.null(by)) && (is.vector(by))) {
    if (length(by) != length(x)) {
      stop("The number of elements in x and by do not match")
    }
  }
  
  if ((!is.null(by)) && (is.data.frame(by) || is.matrix(by))) {
    if (nrow(by) != length(x)) {
      stop("The number of elements in x and by do not match")
    }
  }
  
  if (binary) dist <- "binary"
  
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
    if(any(grepl("of the first", w))) invokeRestart("muffleWarning")
  }
  
  out <- function(x, d){
    x.dist <- dist(x, method = d)
    for (i in 1:(floor(nrow(x)/2)-1)){ # search for non-singular cov matrix
      fit <- withCallingHandlers(cmdscale(x.dist, k = floor(nrow(x)/2)-(i-1)), warning = h)
      out <- TryCatch(out <- .adapt.out(x = fit, ...)$outliers)
      if(!is.null(out[[3]])) {next}
      else {break}
    }
    return(out$value)
  } 
  
  int <- intensityMatrix(x)
  if (!binary){
    int[is.na(int)] <- 0
    dist <- "euclidean"
  }
  if (binary){
    int <- ifelse(is.na(int), 0, 1)
    dist <- "binary"
  }
  int <- as.data.frame(int)
  
  if (is.null(by)){
    outs <- out(int, dist)
  }
  else{
    by <- droplevels(as.factor(by))
    by <- factor(by, levels = unique(by)) # To keep original order
    outs <- do.call(c, lapply(split(int,by), FUN = out, d = dist))
    outs <- data.frame(Group = by, Atypical = outs)
    rownames(outs) <- NULL
  }
  
  return(outs)
  
}