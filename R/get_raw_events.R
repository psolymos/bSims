.get_raw_events <- function(x) {
  if (sum(x$abundance) == 0 || is.null(x$events)) {
    z <- data.frame(
      x=numeric(0),
      y=numeric(0),
      t=numeric(0),
      v=numeric(0),
      i=numeric(0))
  } else {
    z <- lapply(seq_len(length(x$events)), function(i) {
      zz <- x$events[[i]]
      zz$i <- rep(i, nrow(zz))
      zz
    })
    z <- do.call(rbind, z)
    z <- z[order(z$t),]
    rownames(z) <- NULL
  }
  z
}
