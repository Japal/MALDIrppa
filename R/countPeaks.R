countPeaks <-
function(x){
  unlist(lapply(x,function(x) length(mass(x))))
}
