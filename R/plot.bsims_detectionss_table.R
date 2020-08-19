plot.bsims_detections_table <- function(x,
type=c("time", "distance"), xlab, ylab, xlim, ylim,
col_det_vocal, col_det_move, ...) {
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
    xlim <- attr(x, "tlim")
  if (missing(ylim))
    ylim <- c(1, attr(x, "n"))
  if (missing(col_det_vocal))
    col_det_vocal <- getOption("bsims_theme")$col_det_vocal
  if (missing(col_det_move))
    col_det_move <- getOption("bsims_theme")$col_det_move

  plot(o ~ var, tmp, type="n", ylab=ylab, xlab=xlab,
    ylim=ylim, xlim=xlim, ...)
  for (i in tmp$o) {
    tmp2 <- x[x$i == x1$i[i],]
    col <- ifelse(tmp2$v > 0, col_det_vocal, col_det_move)
    lines(c(tmp2$var[1], 10), c(i,i), col="grey")
    points(tmp2$var, rep(i, nrow(tmp2)), cex=0.5, col=col)
    points(tmp2$var[1], i, pch=19, cex=0.5, col=col[1L])
  }
  invisible(x)
}
