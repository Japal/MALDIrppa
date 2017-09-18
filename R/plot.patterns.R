.plot.patterns <- function(a, x, barplot, axis.lab, bar.col, grid, cell.col,
                           cex.axis, cex.lab, prop.col, grid.col, 
                           grid.lty, ...)
{
  
  if (barplot){
    zones <- matrix(c(2, 4, 1, 3), ncol = 2, byrow = TRUE)
    layout(zones, widths = c(5/5, 0/5), heights = c(0.75/5, 4.25/5))
  }
  else {
    zones <- matrix(c(2, 4, 1, 3), ncol = 2, byrow = TRUE)
    layout(zones, widths = c(5/5, 0/5), heights = c(0/5, 5/5))   
  }
  par(mar = c(3, 2, 0.5, 0.5))
  a <- as.matrix(a[rev(rownames(a)),])
  image(1:ncol(a), 1:nrow(a), t(a), col = rev(cell.col), axes = F)
  mtext(side = 1, text = axis.lab[1], line = 1.75, cex = cex.lab)
  mtext(side = 2, text = axis.lab[2], line = 1, cex = cex.lab)
  par(mgp=c(3, .3, 0))
  colnames(a) <- round(as.numeric(colnames(a)), 2)
  axis(side = 1, at = seq(1,ncol(a), by = 1), labels = colnames(a), tck = 0,
       cex.axis = cex.axis, las = 2)
  axis(side = 2, at = seq(1,nrow(a), by = 1), labels = rownames(a), las = 2,
       tck = 0, cex.axis = cex.axis)
  box()
  if (grid){
    grid(ncol(a),nrow(a), col = grid.col, lty = grid.lty)
  }
  
  if (barplot){
    par(mar = c(0, 2, 0.5, 0.5))
    a <- barplot(as.vector(prop.col), axes = F, col = bar.col, xaxs = "i",
                 ylim = c(0, max(as.vector(prop.col)+0.2*max(as.vector(prop.col)))))
  }
} 
