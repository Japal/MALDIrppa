screenSpectra <- function(x,meta=NULL,threshold=1.5,estimator=c("Q","MAD"),
                          method=c("adj.boxplot","boxplot","ESD","Hampel","RC"),
                          nd=1,lambda=0.5,...){

  spobjname <-deparse(substitute(x))
  estimator <- match.arg(estimator)
  method <- match.arg(method)
  smax <- 100 # re-scaling constant
  
  if (any(inherits(x,"list") & inherits(x[[1]],"MassSpectrum"))==FALSE) {
    stop("x must be a list of MassSpectra class objects")
  }
  if ((!is.null(meta)) & (is.vector(meta))){
    if (length(meta) != length(x)){
      stop("The number of elements in x and meta do not match") 
    } 
  }
  if ((!is.null(meta)) & (is.data.frame(meta) | is.matrix(meta))){
    if (nrow(meta) != length(x)){
      stop("The number of elements in x and meta do not match") 
    } 
  }
  if (method%in%c("boxplot","adj.boxplot","ESD","Hampel","RC")==FALSE){
    stop(paste(method,"is not a valid method"))
  }
  if (estimator%in%c("Q","MAD")==FALSE){
    stop(paste(estimator,"is not a valid estimator"))
  }
  if (nd > 5){
    stop("nd must be lower than 6")
  }
  
  scale.max <- function(x,smax){
    
    x <- transfIntensity(x, fun=function(x) x/(max(x)/smax))
    return(x)
  }

  d.spectra <- function(x,nd){

    x <- transfIntensity(x, fun=function(x) sgolayfilt(x, m = nd))
    return(x)  
  }

  sc.filter <- function(spectra,meta=NULL,est.table){
    
    f <- which(est.table$Class != "success")
    if (length(f) != 0){
      fSpectra <- spectra[-f]
      if (!is.null(meta)){
        fType <- as.data.frame(meta)[-f,]
        rownames(fType) <- NULL
        out <- list(fSpectra,fType)
      }
      else {out <- fSpectra}  
    }
    else {
      if (!is.null(meta)){
        out <- list(fSpectra=spectra,fType=meta)
      }
      else {out <- spectra}
    }
    return(out)
  }
  
  if (estimator == "Q"){
    estim <- "Q"
    est  <- unlist(lapply(x,function(y) Qn(intensity(d.spectra(scale.max(y,smax),nd)))))
    med.int <- summarySpectra(x)[,"Med.Int"]
    est <- (est^lambda)*(1/sqrt((med.int+1)))^(1-lambda)
  }
  else{
    estim <- "MAD"
    est <- unlist(lapply(x,function(y) mad(intensity(d.spectra(scale.max(y,smax),nd)))))
    med.int <- summarySpectra(x)[,"Med.Int"]
    est <- (est^lambda)*(1/sqrt((med.int+1)))^(1-lambda)
  }

  if (method == "boxplot"){
    met <- "boxplot"
    t <- quantile(est,probs=0.75,na.rm=T) + threshold*IQR(est,na.rm=T)
    l <- quantile(est,probs=0.25,na.rm=T) - threshold*IQR(est,na.rm=T)}
  if (method == "adj.boxplot"){
    met <- "adj.boxplot"
    t <- adjboxStats(est,coef=threshold)$fence[2]
    l <- adjboxStats(est,coef=threshold)$fence[1]}
  if (method == "ESD"){
    met <- "ESD"
    t <- mean(est,na.rm=T) + threshold*sd(est,na.rm=T)
    l <- mean(est,na.rm=T) - threshold*sd(est,na.rm=T)}
  if (method == "Hampel"){
    met <- "Hampel"
    t <- median(est,na.rm=T) + threshold*mad(est,na.rm=T)
    l <- median(est,na.rm=T) - threshold*mad(est,na.rm=T)}
  if (method == "RC"){
    met <- "RC"
    t <- median(est,na.rm=T) + threshold*Qn(est[!is.na(est)])
    l <- median(est,na.rm=T) - threshold*Qn(est[!is.na(est)])}
  
  names(t) <- NULL; names(l) <- NULL
  
  est.table <- data.frame(ID=names(est))
  est.table["A score"] <- est
  est.table["Class"] <- "success"
  est.table$Class[which(est > t | est < l | is.na(est))] <- "failure"
  
  cfailure <- sum(est.table$Class=="failure")
  prop <- round(cfailure/length(x),4)
  
  if (is.null(meta)){
    x <- sc.filter(x,meta=NULL,est.table)
    }
  if (!is.null(meta)){
    x <- sc.filter(x,meta,est.table)[[1]]
    meta <- sc.filter(x,meta,est.table)[[2]]
    }
    
  out <- list(fspectra=x,fmeta=meta,est.table=est.table,upper=t,lower=l,prop=prop,
              cfailure=cfailure,threshold=threshold,
              nd=nd,lambda=lambda,smax=smax,method=met,estimator=estim,spobjname=spobjname)
  
  class(out) <- "scSpectra"
  return(out)
}
