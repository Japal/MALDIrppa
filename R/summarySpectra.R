summarySpectra <-
function(spec){
  class <- sapply(spec,function(x) class(x)) 
  NmzVal <- sapply(spec,function(x) length(x@mass))
  RmzVal <- sapply(spec,function(x) paste(min(x@mass),"-",max(x@mass),sep=""))
  RintVal <- sapply(spec,function(x) paste(round(min(x@intensity),2),"-",round(max(x@intensity),2),sep=""))
  return(data.frame(Class=class,Number_mz_values=NmzVal,Range_mz_values=RmzVal,
                    Range_intensity_values=RintVal))
}
