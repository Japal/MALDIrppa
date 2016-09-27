addMetadata <- function(x,metadata,pos){

  for (i in 1:length(x)){
    x[[i]]@metaData[[pos]] <- as.character(metadata[i])
  } 
  as.list(x)
}
