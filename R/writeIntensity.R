writeIntensity <- function(x,filename="intMatrix",format=c("R","csv","NEXUS","FASTA"),binary=FALSE, labels=NULL, weights=NULL,...){
  
  if (!is.matrix(x) && !is.data.frame(x)) stop("x must be matrix or data.frame.")
  
  nam <- deparse(substitute(x))
  format <- match.arg(format)
  
  exportNexus <-
    function (x, file, format = "standard", datablock = TRUE, interleaved = FALSE, 
              charsperline = NULL, gap = NULL, missing = NULL, ...) 
    {
        
      if (!is.null(labels)){
          if (is.data.frame(labels)) labels <- labels[,1] # Convert to vector
        labels <- gsub(" ","_",labels)
        x <- split(x,labels) # Add labels to samples
      }
      else x <- split(x,1:nrow(x))
      
      
      indent <- "  "
      maxtax <- 5
      defcharsperline <- 80
      defgap <- "-"
      defmissing <- "?"
      ntax <- length(x)
      nchars <- length(x[[1]])
      zz <- file(file, "w")
      if (is.null(names(x))) {
        names(x) <- as.character(1:ntax)
      }
      "fcat" <- function(..., file = zz) {
        cat(..., file = file, sep = "", append = TRUE)
      }
      "fcat2" <- function(..., file = zz) {
        cat(..., file = file, sep = " ", append = TRUE)
      }
      "find.max.length" <- function(x) {
        max <- 0
        for (i in 1:length(x)) {
          val <- length((strsplit(x[i], split = NULL))[[1]])
          if (val > max) {
            max <- val
          }
        }
        max
      }
      
      "print.matrix" <- function(x, dindent = "    ") {
        Names <- names(x)
        printlength <- find.max.length(Names) + 2
        if (interleaved == FALSE) {
          for (i in 1:length(x)) {
            sequence <- paste(x[[i]], collapse = "")
            taxon <- Names[i]
            thestring <- sprintf("%-*s%s%s", printlength, 
                                 taxon, dindent, sequence)
            fcat(indent, indent, thestring, "\n")
          }
        }
        else {
          ntimes <- ceiling(nchars/charsperline)
          start <- 1
          end <- charsperline
          for (j in 1:ntimes) {
            for (i in 1:length(x)) {
              sequence <- paste(x[[i]][start:end], collapse = "")
              taxon <- Names[i]
              thestring <- sprintf("%-*s%s%s", printlength, 
                                   taxon, dindent, sequence)
              fcat(indent, indent, thestring, "\n")
            }
            if (j < ntimes) {
              fcat("\n")
            }
            start <- start + charsperline
            end <- end + charsperline
            if (end > nchars) {
              end <- nchars
            }
          }
        }
      }
      fcat("#NEXUS\n[File created on", " ", 
           date(), "]\n")
      NCHAR <- paste("NCHAR=", nchars, sep = "")
      NTAX <- paste("NTAX=", ntax, sep = "")
      if (format == "standard") {
        DATATYPE <- "DATATYPE=STANDARD"
      }
      if (is.null(charsperline)) {
        if (nchars < defcharsperline) {
          charsperline <- nchars
          interleaved <- FALSE
        }
        else {
          if (nchars > defcharsperline) {
            charsperline <- defcharsperline
          }
        }
      }
      if (is.null(missing)) {
        MISSING <- paste("MISSING=", defmissing, sep = "")
      }
      else {
        MISSING <- paste("MISSING=", missing, sep = "")
      }
      if (is.null(gap)) {
        GAP <- paste("GAP=", defgap, sep = "")
      }
      else {
        GAP <- paste("GAP=", gap, sep = "")
      }
      if (interleaved == TRUE) {
        INTERLEAVE <- "INTERLEAVE=YES"
      }
      if (interleaved == FALSE) {
        INTERLEAVE <- "INTERLEAVE=NO"
      }
      if (datablock == TRUE) {
        fcat("BEGIN DATA;\n")
        fcat(indent, "DIMENSIONS", " ", NTAX, " ", NCHAR, ";\n")
        if (format %in% c("dna", "protein","standard")) {
          fcat(indent, "FORMAT", " ", DATATYPE, " ", MISSING, 
               " ", GAP, " ", INTERLEAVE, ";\n")
        }
        if (!is.null(weights)) {
          fcat2(indent, "CHARWEIGHTS ",weights)
          fcat(";\n")
        }
        fcat(indent, "MATRIX\n")
        print.matrix(x)
        fcat(indent, ";\n")
        fcat("END;\n\n")
      }
      else {
        fcat("BEGIN TAXA;\n")
        fcat(indent, "DIMENSIONS", " ", NTAX, ";\n")
        fcat(indent, "TAXLABELS\n")
        fcat(indent, indent)
        j <- 0
        for (i in 1:ntax) {
          fcat(names(x[i]), " ")
          j <- j + 1
          if (i == ntax) {
            fcat("\n", indent, ";\n")
          }
          else {
            if (j == maxtax) {
              fcat("\n", indent, indent)
              j <- 0
            }
          }
        }
        fcat("END;\n\n")
        fcat("BEGIN CHARACTERS;\n")
        fcat(indent, "DIMENSIONS", " ", NCHAR, ";\n")
        if (format %in% c("dna", "protein","standard")) {
          fcat(indent, "FORMAT", " ", MISSING, " ", GAP, " ", 
               DATATYPE, " ", INTERLEAVE, ";\n")
        }
        if (!is.null(weights)) {
          fcat2(indent, "CHARWEIGHTS ",weights)
          fcat(";\n")
        }
        fcat(indent, "MATRIX\n")
        print.matrix(x)
        fcat(indent, ";")
        fcat("\nEND;\n\n")
      }
      close(zz)
    }
  
  exportFasta <- function (x, file, append = FALSE, nbcol = 6, colw = 10, indent = NULL, ...) 
  {
    
    N <- dim(x)
    S <- N[2]
    N <- N[1]
    xx <- vector("list", N)
    for (i in 1:N) xx[[i]] <- x[i, ]
    names(xx) <- rownames(x)
    x <- xx
    rm(xx)
    
    if (!is.null(labels))
      if (is.data.frame(labels)) labels <- labels[,1] # Convert to vector
      names(x) <- labels
    if (is.null(names(x))) 
      names(x) <- as.character(1:N)
    if (is.null(indent)) 
      indent <- 0
    if (is.numeric(indent)) 
      indent <- paste(rep(" ", indent), collapse = "")
    
    zz <- if (append) 
      file(file, "a")
    else file(file, "w")
    on.exit(close(zz))
    
    for (i in 1:N) {
      cat(">", names(x)[i], file = zz, sep = "")
      cat("\n", file = zz)
      X <- paste(x[[i]], collapse = "")
      S <- length(x[[i]])
      totalcol <- ceiling(S/colw)
      if (nbcol < 0) nbcol <- totalcol
      nb.lines <- ceiling(totalcol/nbcol)
      SEQ <- character(totalcol)
      for (j in 1:totalcol) SEQ[j] <- substr(X, 1 + (j - 
                                                       1) * colw, colw + (j - 1) * colw)
      for (k in 1:nb.lines) {
        endsel <- if (k == nb.lines) length(SEQ) else nbcol + 
          (k - 1) * nbcol
        cat(indent, file = zz)
        cat(SEQ[(1 + (k - 1) * nbcol):endsel], sep = "", 
            file = zz)
        cat("\n", file = zz)
      }
    }
  }
  
  colnames(x) <- as.character(round(as.numeric(colnames(x)),4))
  x[is.na(x)] <- 0 # Use 0 to indicate no peak
  if ((binary==TRUE) | (format=="NEXUS")) {x <- (x!=0)*1}
  if ((binary==TRUE) | (format=="FASTA")) {x <- (x!=0)*1}
  
  assign(nam,x)   
  
  switch(format,
         R=save(list=eval(nam),file=paste(filename,".Rdata",sep=""), ...),
         csv=write.table(x,file=paste(filename,".csv",sep=""),sep=",",row.names=FALSE, ...),
         NEXUS=exportNexus(x,file=paste(filename,".nex",sep=""), ...),
         FASTA=exportFasta(x,file=paste(filename,".fas",sep=""), ...)
         )
}
