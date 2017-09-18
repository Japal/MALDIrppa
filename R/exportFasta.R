.exportFasta <- function (x, file, append = FALSE, nbcol = 6, colw = 10, indent = NULL, 
                          labels, ...) 
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