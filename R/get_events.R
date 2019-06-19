get_events <-
function(x, event_type=c("vocal", "move", "both"), tlim=NULL) {
  event_type <- match.arg(event_type)
  if (sum(x$abundance) == 0)
    return(data.frame(
      x=numeric(0),
      y=numeric(0),
      t=numeric(0),
      v=numeric(0),
      i=numeric(0)
    ))
  if (is.null(tlim))
    tlim <- c(0, x$duration)
  tlim <- pmin(pmax(0, tlim), x$duration)
  z <- lapply(1:length(x$events), function(i) {
    zz <- x$events[[i]]
    zz$i <- rep(i, nrow(zz))
    zz
  })
  z <- do.call(rbind, z)
  z <- z[order(z$t),]
  keep <- switch(event_type,
    "vocal"=z$v > 0,
    "move"=z$v == 0,
    "both"=rep(TRUE, nrow(z)))
  rownames(z) <- NULL
  z$x <- x$nests$x[z$i] + z$x
  z$y <- x$nests$y[z$i] + z$y
  z <- z[z$t %[]% tlim,,drop=FALSE]
  attr(z, "event_type") <- event_type
  z
}
