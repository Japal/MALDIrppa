writeIntensity <- function(x, filename = "intMatrix", format = c("R", "csv", "NEXUS", "FASTA"),
                           binary = FALSE, labels = NULL, weights = NULL, ...){
  
  if (!is.matrix(x) && !is.data.frame(x)) stop("x must be matrix or data.frame.")
  
  if (!is.null(labels)){
    if (dim(x)[1] != length(labels)) stop("the length of labels does not match the number of rows of x.")
  }
  
  if (!is.null(weights)){
    if (dim(x)[2] != length(weights)) stop("the length of weights does not match the number of columns of x.")
  }
  
  nam <- deparse(substitute(x))
  format <- match.arg(format)
  
  colnames(x) <- as.character(round(as.numeric(colnames(x)), 4))
  x[is.na(x)] <- 0 # Use 0 to indicate no peak
  if ((binary) || (format=="NEXUS")) {x <- (x!=0)*1}
  if ((binary) || (format=="FASTA")) {x <- (x!=0)*1}
  
  assign(nam,x)   
  
  switch(format,
         R = save(list = eval(nam), file = paste(filename, ".Rdata", sep = ""), ...),
         csv = write.table(x, file = paste(filename, ".csv", sep = ""), sep = ",", row.names = FALSE, ...),
         NEXUS = .exportNexus(x, file = paste(filename, ".nex", sep = ""), labels = labels, weights = weights, ...),
         FASTA = .exportFasta(x, file = paste(filename, ".fas", sep = ""), labels = labels, ...)
         )
}
