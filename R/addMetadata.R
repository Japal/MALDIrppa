addMetadata <- function(x, metadata, pos){

  for (i in 1:length(x)){
    metaData(x[[i]])[[pos]] <- as.character(metadata[i])
  } 
  return(as.list(x))

}
