plot.bsims_events_table <- function(x, ...) {
  x1 <- x[!duplicated(x$i),]
  tmp <- x1
  tmp$o <- seq_len(nrow(x1))
  ylab <- if (is.null(list(...)$ylab))
    "Individuals" else list(...)$ylab
  xlab <- if (is.null(list(...)$xlab))
    "Time" else list(...)$xlab
  xlim <- if (is.null(list(...)$xlim))
    attr(x, "tmil") else list(...)$xlim
  plot(o ~ t, tmp, type="n", ylab=ylab, xlim=xlim, ...)
  for (i in tmp$o) {
    tmp2 <- x[x$i == x1$i[i],]
    lines(c(tmp2$t[1], 10), c(i,i), col="grey")
    points(tmp2$t, rep(i, nrow(tmp2)), cex=0.5)
    points(tmp2$t[1], i, pch=19, cex=0.5)
  }
  invisible(x)
}
