lines.bsims_detections <-
function(x, first_only=TRUE, tlim=NULL, ...) {
  if (is.null(tlim))
    tlim <- c(0, x$duration)
  z <- get_detections(x, first_only)
  z <- z[z$t %[]% tlim,,drop=FALSE]
  xy <- z[,c("x", "y")]
  #segments(
  #  x0=rep(x$xy[1L], nrow(xy)),
  #  y0=rep(x$xy[2L], nrow(xy)),
  #  x1=xy[,1L], y1=xy[,2L], ...)
  for (i in seq_len(nrow(z))) {
    lines(c(x$xy[1L], xy[i,1L]), c(x$xy[2L], xy[i,2L]), ...)
  }
  invisible(x)
}
