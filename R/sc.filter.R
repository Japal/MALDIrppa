.sc.filter <- function(spectra, meta, est.table){
  
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
      out <- list(fSpectra = spectra, fType = meta)
    }
    else {out <- spectra}
  }
  
  return(out)

}