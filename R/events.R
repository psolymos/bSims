events <-
function(vocal_rate=1, move_rate=1,
duration=10, movement=0, avoid=c(0,0)) {
  ev <- cumsum(timetoevent(vocal_rate, duration))
  em <- cumsum(timetoevent(move_rate, duration))
  iv <- rep(1, length(ev))
  im <- rep(0, length(em))
  dv <- matrix(NA, length(ev), 2)
  dm <- rmvn(length(em), c(0, 0), diag(movement^2, 2, 2))

  dm <- dm[dm[,1] %][% avoid,,drop=FALSE]
  while (nrow(dm) < length(em)) {
    dm <- rbind(dm, rmvn(length(em), c(0, 0), diag(movement^2, 2, 2)))
    dm <- dm[dm[,1] %][% avoid,,drop=FALSE]
  }
  dm <- dm[seq_along(em),,drop=FALSE]
  h <- cbind(rbind(dv, dm), c(ev, em), c(iv, im))
  colnames(h) <- c("x", "y", "t", "v")
  o <- order(h[,"t"])
  h <- as.data.frame(h[o,,drop=FALSE])
  ## take previous known location for vocalizations
  for (i in which(is.na(h[,"x"]))) {
    if (i == 1L) {
      h$x[i] <- 0
      h$y[i] <- 0
    } else {
      h$x[i] <- h$x[i-1L]
      h$y[i] <- h$y[i-1L]
    }
  }
  h[h$t <= duration,,drop=FALSE]
}
