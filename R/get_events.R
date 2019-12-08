.get_events <-
function(x, event_type=c("vocal", "move", "both"), tlim=NULL) {
  z <- .get_raw_events(x)
  if (nrow(z) == 0)
    return(z)
  event_type <- match.arg(event_type)
  if (is.null(tlim))
    tlim <- c(0, x$duration)
  if (length(tlim) != 2)
    stop("tlim must be a vector of length 2")
  tlim <- pmin(pmax(0, tlim), x$duration)
  keep <- switch(event_type,
    "vocal"=z$v > 0,
    "move"=z$v == 0,
    "both"=rep(TRUE, nrow(z)))
  z <- z[keep,,drop=FALSE]
  z$x <- x$nests$x[z$i] + z$x
  z$y <- x$nests$y[z$i] + z$y
  z <- z[z$t %[)% tlim,,drop=FALSE]
  attr(z, "event_type") <- event_type
  attr(z, "tlim") <- tlim
  attr(z, "n") <- length(x$events)
  class(z) <- c("bsims_events_table", "data.frame")
  z
}
