rmvn <-
function(n=1L, mu, Sigma, ...) {
  if (n < 2L) {
    if (n == 0L) {
      out <- matrix(numeric(0), nrow=0L, ncol=length(mu))
    } else {
      out <- matrix(MASS::mvrnorm(n, mu, Sigma, ...), nrow=1L)
    }
    colnames(out) <- names(mu)
  } else {
    out <- MASS::mvrnorm(n, mu, Sigma, ...)
  }
  out
}
