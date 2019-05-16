timetoevent <-
function(rate, duration) {
  if (rate == Inf)
    return(rexp(1, rate))
  if (rate == 0)
    rate <- .Machine$double.eps
  te <- rexp(n=ceiling(duration*rate), rate=rate)
  while(sum(te) < duration) {
    te <- c(te, rexp(n=ceiling(duration*rate), rate=rate))
  }
  cte <- cumsum(te) < duration
#  if (sum(cte) < 1)
#    te[1] else te[cte]
  te[cte]
}
