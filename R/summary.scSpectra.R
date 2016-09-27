summary.scSpectra <- function(object,ncases=10, ...){
  
  if (inherits(object,"scSpectra")==FALSE) {
    stop("object must be a scSpectra class object")
  }
  
  if (inherits(ncases,"numeric")==FALSE) {
    stop("ncases must be a valid number")
  }
  
  cat(paste(object$estimator," estimator from the scaled ",object$nd,"-derivative:\n (",ncases," first mass spectra) \n",sep=""))
  print(head(object$est.table,ncases))
  cat("\n");cat("----------------------------")
  cat("\n\n")
  cat(paste("Scale estimator:",object$estimator,"\n"))
  cat(paste("Method:",object$met,"\n"))
  cat(paste("Threshold:",object$threshold,"\n"))
  cat(paste("Limits: [",round(object$lower,4),",",round(object$upper,4),"] \n",sep=""))
  cat(paste("Deriv. order:",object$nd,"\n"))
  cat(paste("No. potentially faulty spectra: ",object$cfailure," (",object$prop*100," %)",sep=""))
}
