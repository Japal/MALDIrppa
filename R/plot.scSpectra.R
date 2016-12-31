plot.scSpectra <- function(x,type=c("index","hist"),breaks=30,labels=FALSE,col="green3", ...){
  
  type <- match.arg(type)
  
  if (inherits(x,"scSpectra")==FALSE) {
    stop("x must be a scSpectra class object")
  }
  if (type%in%c("index","hist")==FALSE){
    stop(paste(type,"is not a valid plot type"))
  }
  
  u <- as.numeric(x$upper)
  l <- as.numeric(x$lower)
  if (labels[1]==TRUE) {labels <- rownames(x$est.table)}
  else {labels <- labels}
  cols <- ifelse(x$est.table[,"Score"]<=l | x$est.table[,"Score"] >= u,"red3","blue3")
  
  if (type=="index"){
    xyplot(est.table[,"Score"] ~ 1:nrow(est.table),data=x,cex=0.75,col="white",
           ylab=paste(x$nd,"-DSS ",x$estimator," estimator",sep=""),xlab="Index",
           panel = function(x,y,...){
             panel.xyplot(x,y,...)
             panel.abline(h=u,lty=3)
             panel.abline(h=l,lty=3)
             if (length(labels) > 1){
              ltext(x,y,labels=labels,cex=0.8,col=cols)
             }
             else{
              panel.xyplot(x,y,pch=1,col=cols)
             }
           }
           ,...)
  }
  else {
    histogram(x$est.table[,"Score"],breaks=breaks,col=col,
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
