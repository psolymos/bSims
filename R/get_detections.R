get_detections <-
function(x, first_only=TRUE) {
  if (sum(x$abundance) == 0)
    return(data.frame(
      x=numeric(0),
      y=numeric(0),
      t=numeric(0),
      v=numeric(0),
      d=numeric(0),
      i=numeric(0)
    ))
  z <- lapply(1:length(x$events), function(i) {
    zz <- x$events[[i]]
    zz$i <- rep(i, nrow(zz))
    zz <- zz[!is.na(zz$d),,drop=FALSE]
    zz
  })
  z <- do.call(rbind, z)
  z <- z[order(z$t),]
  if (first_only)
    z <- z[!duplicated(z$i),,drop=FALSE]
  rownames(z) <- NULL
  z$x <- x$nests$x[z$i] + z$x
  z$y <- x$nests$y[z$i] + z$y
  ## angle in degrees counter clockwise from x axis right
  z$a <- 180 * atan2(z$x, z$y) / pi
  z$a[z$a < 0] <- 360+z$a[z$a < 0]
  ## observer position
  attr(z, "observer") <- x$xy
  z
}
