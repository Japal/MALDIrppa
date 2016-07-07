writeMetadata <-
function(x,file="type",format=c("R","csv"),...){
  
  # x: metadata matrix
  # file: output file name
  # format: output file format
  
  switch(format,
         R=save(x,file=paste(file,".Rdata",sep="")),
         csv=write.table(x,file=paste(file,".csv",sep=""),sep=",",row.names = FALSE))
}
