plot.scSpectra <-
function(x,plot=c("index","hist"),breaks=30,col="green3",...){
  
  plot <- match.arg(plot)
  
  if (inherits(x,"scSpectra")==FALSE) {
    stop("x must be a scSpectra class object")
  }
  if (plot%in%c("index","hist")==FALSE){
    stop(paste(plot,"is not a valid plot"))
  }
  
  u <- as.numeric(x$upper)
  l <- as.numeric(x$lower)
  labels <- x$est.table$label
  cols <- ifelse(x$est.table[,"value"]<=l | x$est.table[,"value"] >= u,"red3","blue3")
  
  if (plot=="index"){
    xyplot(est.table[,"value"] ~ 1:nrow(est.table),data=x,cex=0.75,col="white",
           ylab=paste(x$nd,"-DSS ",x$estimator," estimator",sep=""),xlab="Index",
           panel = function(x,y,...){
             panel.xyplot(x,y,...)
             panel.abline(h=u,lty=3)
             panel.abline(h=l,lty=3)
             ltext(x,y,labels=labels,cex=0.8,col=cols)
           }
           ,...)
  }
  else {
    histogram(x$est.table[,"value"],breaks=breaks,col=col,
              xlab=paste(x$nd,"-DSS ",x$estimator," estimator",sep=""),
              ylab=list("Percent of total"),
              panel = function(x,...){
                panel.histogram(x,...)
                panel.abline(v=u,lty=3)
                panel.abline(v=l,lty=3)
              }
              ,...)
  }
}
