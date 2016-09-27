writeMetadata <- function(x,filename="Metadata",format=c("R","csv"),...){
  
  nam <- deparse(substitute(x))
  format <- match.arg(format)
  
  assign(nam,x)
  
  switch(format,
         R=save(list=eval(nam),file=paste(filename,".Rdata",sep="")),
         csv=write.table(x,file=paste(filename,".csv",sep=""),sep=",",row.names = FALSE))
}
