estimate <- function (object, ...)
  UseMethod("estimate")

estimate.bsims_transcript <- function (object, method = c("qpad", "sqpad", "convolution", "naive"), ...) {
  method <- match.arg(method)
  FUN <- switch(method,
    "naive" = estimate_naive,
    "qpad" = estimate_qpad,
    "sqpad" = estimate_sqpad,
    "convolution" = estimate_conv,
    stop("Method not found"))
  FUN(object, ...)
}

estimate_qpad <- function (object, ...) {
  if (!requireNamespace("detect"))
    stop("Use `install.packages('detect')` to install the 'detect' package.")
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
  c(phi=phihat, tau=tauhat, density=Dhat, area=A)
}

estimate_naive <- function (object, ...) {
  rint <- object$rint
  y <- get_table(object, "removal")
  if (any(is.infinite(rint))) {
    y <- y[!is.infinite(rint),,drop=FALSE]
    rint <- rint[!is.infinite(rint)]
  }
  rmax <- max(rint)
  A <- pi * rmax^2
  Dhat <- sum(y) / A
  c(phi=NA_real_, tau=NA_real_, density=Dhat, area=A)
}

estimate_sqpad <- function (object, ...) {

  if (!requireNamespace("detect"))
    stop("Use `install.packages('detect')` to install the 'detect' package.")

  y <- get_table(object, "removal")
  rint <- object$rint
  tint <- object$tint

  if (any(is.infinite(rint))) {
    y <- y[!is.infinite(rint),,drop=FALSE]
    rint <- rint[!is.infinite(rint)]
  }
  rmax <- max(rint)
  A <- pi * rmax^2

  d <- data.frame(y=numeric(0L), dis=numeric(0L), dur=numeric(0L))
  for (i in seq_along(rint)) {
    for (j in seq_along(tint)) {
      r <- data.frame(
        y=sum(y[seq_len(i), seq_len(j)]), dis=rint[i], dur=tint[j])
      d <- rbind(d, r)
    }
  }

  m <- try(detect::sqpad.fit(
      Y=d$y, dis=d$dis, dur=d$dur, 
      type="full", 
      det=if (object$condition == "det1") "joint" else "pq",
      ...),
    silent=TRUE)
  if (!inherits(m, "try-error")) {
    est <- unname(exp(stats::coef(m)))
    c(phi=est[2], tau=est[3], density=est[1], area=A)
  } else {
    c(phi=NA_real_, tau=NA_real_, density=NA_real_, area=A)
  }
}

estimate_conv <- function (object, ...) {

  if (!requireNamespace("detect"))
    stop("Use `install.packages('detect')` to install the 'detect' package.")

  rint <- object$rint
  tint <- object$tint

  if (any(is.infinite(rint))) {
    rint <- rint[!is.infinite(rint)]
  }
  rmax <- max(rint)
  A <- pi * rmax^2
  tmax <- max(tint)

  dets <- get_detections(object)
  dets <- dets[dets$t <= tmax & dets$d <= rmax,]
  dlist <- list(dets$d)

  m <- try(detect::sqpad.fit(
      Y=nrow(dets), dis=rmax, dur=tmax,
      dislist=dlist, 
      type="conv", 
      det=if (object$condition == "det1") "joint" else "pq",
      ...),
    silent=TRUE)
  if (!inherits(m, "try-error")) {
    est <- unname(exp(stats::coef(m)))
    c(phi=est[2], tau=est[3], density=est[1], area=A)
  } else {
    c(phi=NA_real_, tau=NA_real_, density=NA_real_, area=A)
  }
}
