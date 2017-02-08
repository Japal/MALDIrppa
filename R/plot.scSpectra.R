plot.scSpectra <- function(x,type=c("index","hist","casewise"),breaks=30,labels=FALSE,col="green3", ...){
  
  type <- match.arg(type)
  
  if (inherits(x,"scSpectra")==FALSE) {
    stop("x must be a scSpectra class object")
  }
  if (type%in%c("index","hist","casewise")==FALSE){
    stop(paste(type,"is not a valid plot type"))
  }
  
  u <- as.numeric(x$upper)
  l <- as.numeric(x$lower)
  if (labels[1]==TRUE) {labels <- rownames(x$est.table)}
  else {labels <- labels}
  cols <- ifelse(x$est.table[,"A score"]<=l | x$est.table[,"A score"] >= u,"red3","blue3")
  
  switch(type,
         index = {
           xyplot(
             est.table[, "A score"] ~ 1:nrow(est.table),
             data = x,
             cex = 0.75,
             col = "white",
             ylab = "A score",
             xlab = "Index",
             panel = function(x, y, ...) {
               panel.xyplot(x, y, ...)
               panel.abline(h = u, lty = 3)
               panel.abline(h = l, lty = 3)
               if (length(labels) > 1) {
                 ltext(x,
                       y,
                       labels = labels,
                       cex = 0.8,
                       col = cols)
               }
               else{
                 panel.xyplot(x, y, pch = 1, col = cols)
               }
             }
             ,
             ...
           )
         },
         hist = {
           histogram(
             x$est.table[, "A score"],
             breaks = breaks,
             col = col,
             xlab = "A score",
             ylab = list("Percentage frequency"),
             panel = function(x, ...) {
               panel.histogram(x, ...)
               panel.abline(v = u, lty = 3)
               panel.abline(v = l, lty = 3)
             }
             ,
             ...
           )
         },
         casewise = {
           cases <- which(x$est.table$Class == "failure")
           labels <- labels[cases]
           spec <- get(x$spobjname)[cases]
           for (i in 1:length(cases)) {
             plot(spec[[i]], main = labels[i])
             readline(prompt = "Press any key to continue")
           }
         })
}
