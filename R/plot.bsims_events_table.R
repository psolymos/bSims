plot.bsims_events_table <- function(x,
xlab, ylab, xlim, ylim, ...) {
  x1 <- x[!duplicated(x$i),]
  tmp <- x1
  tmp$o <- seq_len(nrow(x1))
  if (missing(xlab))
    xlab <- "Time"
  if (missing(ylab))
    ylab <- "Individuals"
  if (missing(xlim))
    xlim <- attr(x, "tmil")
  if (missing(ylim))
    ylim <- c(1, attr(x, "n"))
  plot(o ~ t, tmp, type="n", ylab=ylab, xlab=xlab,
    ylim=ylim, xlim=xlim, ...)
  for (i in tmp$o) {
    tmp2 <- x[x$i == x1$i[i],]
    lines(c(tmp2$t[1], 10), c(i,i), col="grey")
    points(tmp2$t, rep(i, nrow(tmp2)), cex=0.5)
    points(tmp2$t[1], i, pch=19, cex=0.5)
  }
  invisible(x)
}
