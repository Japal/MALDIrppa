summaryPeaks <-
function(peak){
  class <- sapply(peak,function(x) class(x)) 
  NmzVal <- sapply(peak,function(x) length(x@mass))
  RmzVal <- sapply(peak,function(x) paste(round(min(x@mass),2),"-",round(max(x@mass),2),sep=""))
  RintVal <- sapply(peak,function(x) paste(round(min(x@intensity),2),"-",round(max(x@intensity),2),sep=""))
  RsnrVal <- sapply(peak,function(x) paste(round(min(x@snr),2),"-",round(max(x@snr),2),sep=""))
  return(data.frame(Class=class,Number_peaks=NmzVal,Range_mz_values=RmzVal,
                    Range_intensity_values=RintVal,Range_snr_values=RsnrVal))
}
