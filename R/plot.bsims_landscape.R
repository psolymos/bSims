plot.bsims_landscape <-
function(x, col, xlim=NULL, ylim=NULL, ...) {

  if (missing(col))
    col <- .bsims_theme$col

  A <- diff(x$strata) * diff(range(x$strata))
  A <- c(h=A[1]+A[5], e=A[2]+A[4], r=A[3])
  names(A) <- c("H", "E", "R")
  plot(0, type="n",
    xlim=if (is.null(xlim)) range(x$box[,"x"]) else xlim,
    ylim=if (is.null(ylim)) range(x$box[,"y"]) else ylim,
    xlab="", ylab="", axes=FALSE, asp=1, ...)
  if (A[1] > 0)
    polygon(x$box, col=col[1], border=NA)
  if (A[2] > 0)
    polygon(x$strata[c("he", "he", "eh", "eh")],
      x$strata[c("+h", "h+", "h+", "+h")],
      col=col[2], border=NA)
  if (A[3] > 0)
    polygon(x$strata[c("er", "er", "re", "re")],
      x$strata[c("+h", "h+", "h+", "+h")],
      col=col[3], border=NA)
  invisible(x)
}
