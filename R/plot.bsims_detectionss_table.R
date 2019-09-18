plot.bsims_detections_table <- function(x,
type=c("time", "distance"), xlab, ylab, xlim, ylim, ...) {
  type <- match.arg(type)
  if (type == "distance") {
    x <- x[order(x$d), ]
    x$var <- x$d
  } else {
    x <- x[order(x$t), ]
    x$var <- x$t
  }
  x1 <- x[!duplicated(x$i),]
  tmp <- x1
  tmp$o <- seq_len(nrow(x1))
  if (missing(xlab))
    xlab <- if (type == "distance")
      "Distance" else "Time"
  if (missing(ylab))
    ylab <- "Individuals"
  if (missing(xlim))
    xlim <- attr(x, "tmil")
  if (missing(ylim))
    ylim <- c(1, attr(x, "n"))
  plot(o ~ var, tmp, type="n", ylab=ylab, xlab=xlab,
    ylim=ylim, xlim=xlim, ...)
  for (i in tmp$o) {
    tmp2 <- x[x$i == x1$i[i],]
    lines(c(tmp2$var[1], 10), c(i,i), col="grey")
    points(tmp2$var, rep(i, nrow(tmp2)), cex=0.5)
    points(tmp2$var[1], i, pch=19, cex=0.5)
  }
  invisible(x)
}
