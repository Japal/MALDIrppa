plot.scSpectra <-
function(x,type=c("index","hist"),breaks=30,label=NULL,col="green3",...){
  
  type <- match.arg(type)
  
  if (inherits(x,"scSpectra")==FALSE) {
    stop("x must be a scSpectra class object")
  }
  if (type%in%c("index","hist")==FALSE){
    stop(paste(type,"is not a valid plot type"))
  }
  
  u <- as.numeric(x$upper)
  l <- as.numeric(x$lower)
  if (is.null(label)) {labels <- rownames(x$est.table)}
  else {labels <- label}
  cols <- ifelse(x$est.table[,"Value"]<=l | x$est.table[,"Value"] >= u,"red3","blue3")
  
  if (type=="index"){
    xyplot(est.table[,"Value"] ~ 1:nrow(est.table),data=x,cex=0.75,col="white",
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
    histogram(x$est.table[,"Value"],breaks=breaks,col=col,
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
