screenSpectra <- function(x, meta = NULL, threshold = 1.5, estimator = c("Q", "MAD"),
                          method = c("adj.boxplot", "boxplot", "ESD", "Hampel", "RC"),
                          nd = 1, lambda = 0.5,...){

  spobjname <- deparse(substitute(x))
  estimator <- match.arg(estimator)
  method <- match.arg(method)
  smax <- 100 # re-scaling constant
  
  if (!isMassSpectrumList(x)) {
    stop("x must be a list of MassSpectra class objects")
  }
  
  if ((!is.null(meta)) && (is.vector(meta) || is.factor(meta))){
    if (length(meta) != length(x)){
      stop("The number of elements in x and meta do not match") 
    } 
  }
  
  if ((!is.null(meta)) && (is.data.frame(meta) || is.matrix(meta))){
    if (nrow(meta) != length(x)){
      stop("The number of elements in x and meta do not match") 
    } 
  }
  
  if (!method %in% c("boxplot", "adj.boxplot", "ESD", "Hampel", "RC")){
    stop(paste(method,"is not a valid method"))
  }
  
  if (!estimator%in%c("Q","MAD")){
    stop(paste(estimator,"is not a valid estimator"))
  }
  
  if (nd > 5){
    stop("nd must be lower than 6")
  }
  
  scale.max <- function(x, smax){
    
    x <- transfIntensity(x, fun = function(x) x/(max(x)/smax))
    return(x)
  }

  d.spectra <- function(x, nd){

    x <- transfIntensity(x, fun = function(x) sgolayfilt(x, m = nd))
    return(x)  
  }
  
  if (estimator == "Q"){
    estim <- "Q"
    est  <- unlist(lapply(x, function(y) Qn(intensity(d.spectra(scale.max(y, smax), nd)))))
    med.int <- summarySpectra(x)[, "Med.Int"]
    est <- (est^lambda)*(1/sqrt((med.int+1)))^(1-lambda)
  }
  else{
    estim <- "MAD"
    est <- unlist(lapply(x, function(y) mad(intensity(d.spectra(scale.max(y,smax), nd)))))
    med.int <- summarySpectra(x)[, "Med.Int"]
    est <- (est^lambda)*(1/sqrt((med.int+1)))^(1-lambda)
  }
  
  if (method == "boxplot"){
    met <- "boxplot"
    limits <- quantile(est, probs = c(0.75, 0.25), na.rm = T) + c(threshold, -threshold)*IQR(est, na.rm = T)
  }
  
  if (method == "adj.boxplot"){
    met <- "adj.boxplot"
    limits <- adjboxStats(est, coef = threshold)$fence[c(2,1)]
  }
  
  if (method == "ESD"){
    met <- "ESD"
    limits <- mean(est, na.rm = T) + c(threshold, -threshold)*sd(est, na.rm = T)
  }
  
  if (method == "Hampel"){
    met <- "Hampel"
    limits <- median(est, na.rm = T) + c(threshold, -threshold)*mad(est, na.rm = T)
  }
  
  if (method == "RC"){
    met <- "RC"
    limits <- median(est, na.rm = T) + c(threshold, -threshold)*Qn(est[!is.na(est)])
  }
  
  t <- limits[1]; l <- limits[2]
  
  if (!is.null(names(est))) ID <-  names(est) else ID <- 1:length(est)
  
  est.table <- data.frame(ID = ID)
  est.table["A score"] <- est
  est.table["Class"] <- "success"
  est.table$Class[which(est > t | est < l | is.na(est))] <- "failure"
  
  cfailure <- sum(est.table$Class == "failure")
  prop <- round(cfailure/length(x),4)
  
  if (is.null(meta)){
    x <- .sc.filter(spectra = x, meta = meta, est.table = est.table)
  }
  
  if (!is.null(meta)){
    x <- .sc.filter(spectra = x, meta = meta, est.table = est.table)[[1]]
    meta <- .sc.filter(spectra = x, meta = meta, est.table = est.table)[[2]]
  }
  
  out <- list(fspectra = x, fmeta = meta, est.table = est.table, upper = t,
              lower = l, prop = prop, cfailure = cfailure, threshold = threshold,
              nd = nd, lambda = lambda, smax = smax, method = met,
              estimator = estim, spobjname = spobjname)
  
  class(out) <- "scSpectra"
  
  return(out)
}
