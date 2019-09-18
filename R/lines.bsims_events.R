lines.bsims_events <-
function(x, tlim=NULL, ...) {
  if (is.null(tlim))
    tlim <- c(0, x$duration)
  tlim <- pmin(pmax(0, tlim), x$duration)
  N <- length(x$events)
  for (i in seq_len(N)) {
    xy <- cbind(
      x=x$nests$x[i] + x$events[[i]]$x,
      y=x$nests$y[i] + x$events[[i]]$y)
    xy <- xy[x$events[[i]]$t %[)% tlim,,drop=FALSE]
    lines(xy, ...)
  }
  invisible(x)
}
