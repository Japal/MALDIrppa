.summaryStats <- function(x, digits = 4){
  
  No.MZ <- sapply(x, function(x) length(mass(x)))
  Min.MZ <- sapply(x, function(x) round(min(mass(x)), digits))
  Max.MZ <- sapply(x, function(x) round(max(mass(x)), digits))
  Min.Int <- sapply(x, function(x) round(min(intensity(x)), digits))
  Mean.Int <- sapply(x, function(x) round(mean(intensity(x)), digits))
  Std.Int <- sapply(x, function(x) round(sd(intensity(x)), digits))
  Med.Int <- sapply(x, function(x) round(median(intensity(x)), digits))
  MAD.Int <- sapply(x, function(x) round(mad(intensity(x)), digits))
  Max.Int <- sapply(x, function(x) round(max(intensity(x)), digits))

  return(data.frame(No.MZ,Min.MZ,Max.MZ,Min.Int,Mean.Int,Std.Int,Med.Int,
                    MAD.Int,Max.Int))
  
}