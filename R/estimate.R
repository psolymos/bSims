estimate <- function (object, ...)
  UseMethod("estimate")

estimate.bsims_transcript <- function (object, ...) {
  y <- get_table(object, "removal")
  rmax <- max(object$rint)
  tmax <- max(object$tint)
  if (length(object$tint) > 1 && sum(y) > 0) {
    fitp <- try(detect::cmulti.fit(
      matrix(colSums(y), 1),
      matrix(object$tint, 1),
      type="rem"), silent=TRUE)
    if (!inherits(fitp, "try-error")) {
      phihat <- exp(fitp$coef)
      p <- 1-exp(-tmax*phihat)
    } else {
      phihat <- NA
      p <- NA
    }
  } else {
    phihat <- NA
    p <- NA
  }
  if (length(object$rint) > 1 && sum(y) > 0) {
    fitq <- try(detect::cmulti.fit(
      matrix(rowSums(y), 1),
      matrix(object$rint, 1),
      type="dis"), silent=TRUE)
    if (!inherits(fitq, "try-error")) {
      tauhat <- exp(fitq$coef)
      if (is.infinite(rmax)) {
        A <- pi * tauhat^2
        q <- 1
      } else {
        q <- (tauhat^2/rmax^2) * (1-exp(-(rmax/tauhat)^2))
        A <- pi * rmax^2
      }
    } else {
      tauhat <- NA
      A <- NA
      q <- NA
    }
  } else {
    tauhat <- NA
    A <- NA
    q <- NA
  }
  Dhat <- sum(y) / (A * p * q)
  c(phi=phihat, tau=tauhat, density=Dhat)
}
