dist_fun2 <-
function(d, tau, dist_fun, b=numeric(0)) {
  if (any(is.infinite(b)))
    stop("b must be finite")
  if (length(b) != length(tau)-1L)
    stop("length(b) must equal length(tau)-1")
  if (length(b) != length(unique(b)))
    stop("values in b must be unique")
  b <- sort(b)
  b[b==0] <- .Machine$double.eps
  h <- 1
  for (i in seq_len(length(b))) {
    h <- c(h,
      h[length(h)] * dist_fun(b[i], tau[i]) /
        dist_fun(b[i], tau[i+1L]))
  }
  j <- cut(d, c(0, b, Inf), labels=FALSE, include.lowest=TRUE)
  dist_fun(d, tau[j]) * h[j]
}
