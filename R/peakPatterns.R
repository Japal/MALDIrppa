peakPatterns <- function(x,abs.lab=NA,barplot=TRUE,
                      axis.lab=c("m/z","Index"),
                      bar.color="red3",
                      cell.colors=c("white","dodgerblue"),
                      grid=FALSE,grid.color="black",grid.lty="dotted",
                      cex.axis=0.5,cex.lab=0.5, ...){
  
  
  plot.patterns <- function(a, x = x, barplot = barplot, ...)
  {
    
    if (barplot==TRUE){
      zones <- matrix(c(2,4,1,3), ncol=2, byrow=TRUE)
      layout(zones, widths=c(5/5,0/5), heights=c(0.75/5,4.25/5))
    }
    else {
      zones <- matrix(c(2,4,1,3), ncol=2, byrow=TRUE)
      layout(zones, widths=c(5/5,0/5), heights=c(0/5,5/5))   
    }
    par(mar=c(3,2,0.5,0.5))
    a <- as.matrix(a[rev(rownames(a)),])
    image(1:ncol(a),1:nrow(a),t(a),col=rev(cell.colors),axes=F)
    mtext(side=1,text=axis.lab[1],line=1.75,cex=cex.lab)
    mtext(side=2,text=axis.lab[2],line=1,cex=cex.lab)
    par(mgp=c(3, .3, 0))
    colnames(a) <- round(as.numeric(colnames(a)),2)
    axis(side = 1,at = seq(1,ncol(a),by=1),labels=colnames(a),tck=0,cex.axis=cex.axis,las=2)
    axis(side = 2,at = seq(1,nrow(a),by=1),labels=rownames(a),las=2,tck=0,cex.axis=cex.axis)
    box()
    if (grid==TRUE){
      grid(ncol(a),nrow(a),col=grid.color,lty=grid.lty)
    }
    
    if (barplot==TRUE){
      par(mar=c(0,2,0.5,0.5))
      a <- barplot(as.vector(prop_col),axes=F,col=bar.color,xaxs="i",
                 ylim=c(0,max(as.vector(prop_col)+0.2*max(as.vector(prop_col)))))
    }
  } 
    
  if ((!is.matrix(x)) & (!is.data.frame(x)) & (!is.list(x))) stop("x is not a valid object")
  if (is.null(abs.lab)) stop("A value for abs.lab must be given")
  if (!is.na(abs.lab)){
    if (!any(x==abs.lab,na.rm=TRUE)) stop(paste("Label",abs.lab,"was not found in the data set"))
  }
  
  if ((class(x)=="list") & (class(x[[1]])=="MassPeaks")) {
    x <- intensityMatrix(x)
  }
  
  x <- as.data.frame(x)
  
  n <- nrow(x); p <- ncol(x)
  x[x==abs.lab] <- NA
  
  np <- as.data.frame(is.na(x)*1)

  prop_col <- 100-round(colSums(np)/n*100,2)
  
  plot.patterns(np, x = x, barplot = barplot,...)

}