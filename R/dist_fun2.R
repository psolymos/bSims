dist_fun2 <-
function(d, tau, dist_fun, breaks=numeric(0), ...) {
  breaks <- sort(breaks)
  breaks[breaks==0] <- .Machine$double.eps
  for (i in which(duplicated(breaks))) {
    breaks[i] <- breaks[i-1] + 10*.Machine$double.eps
  }
  if (any(is.infinite(breaks)))
    stop("breaks must be finite")
  if (length(breaks) != length(tau)-1L)
    stop("length(breaks) must equal length(tau)-1")
#  if (length(breaks) != length(unique(breaks)))
#    stop("values in breaks must be unique")
  h <- 1
  for (i in seq_len(length(breaks))) {
    h <- c(h,
      h[length(h)] * dist_fun(breaks[i], tau[i], ...) /
        dist_fun(breaks[i], tau[i+1L]), ...)
  }
  j <- cut(d, c(0, breaks, Inf), labels=FALSE, include.lowest=TRUE)
  dist_fun(d, tau[j], ...) * h[j]
}
