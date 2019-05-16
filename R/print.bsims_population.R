print.bsims_population <-
function(x, ...) {
  A <- diff(x$strata) * diff(range(x$strata))
  A <- c(h=A[1]+A[5], e=A[2]+A[4], r=A[3])
  names(A) <- c("H", "E", "R")
  her <- paste0(
    ifelse(A[1] > 0, "H", ""),
    ifelse(A[2] > 0, "E", ""),
    ifelse(A[3] > 0, "R", ""), collapse="")
  cat("bSims population\n  ",
    round(x$extent/10, 1), " km x ", round(x$extent/10, 1),
    " km\n  stratification: ", her,
    "\n  total abundance: ", sum(x$abundance), "\n", sep="")
  invisible(x)
}
