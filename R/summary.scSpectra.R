summary.scSpectra <-
function(x,ncases=10){
  
  if (inherits(x,"scSpectra")==FALSE) {
    stop("x must be a scSpectra class object")
  }
  
  cat(paste(x$estimator," estimator from the scaled ",x$nd,"-derivative:\n (",ncases," first mass spectra) \n",sep=""))
  print(head(x$est.table,ncases))
  cat("\n");cat("----------------------------")
  cat("\n\n")
  cat(paste("Scale estimator:",x$estimator,"\n"))
  cat(paste("Method:",x$met,"\n"))
  cat(paste("Threshold:",x$threshold,"\n"))
  cat(paste("Limits: [",round(x$lower,4),",",round(x$upper,4),"] \n",sep=""))
  cat(paste("Deriv. order:",x$nd,"\n"))
  cat(paste("No. potentially faulty spectra: ",x$cfailure," (",x$prop*100," %)",sep=""))
}
