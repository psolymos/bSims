print.bsims_events <-
function(x, ...) {
  A <- diff(x$strata) * diff(range(x$strata))
  A <- c(h=A[1]+A[5], e=A[2]+A[4], r=A[3])
  names(A) <- c("H", "E", "R")
  her <- paste0(
    ifelse(A[1] > 0, "H", ""),
    ifelse(A[2] > 0, "E", ""),
    ifelse(A[3] > 0, "R", ""), collapse="")
  cat("bSims events\n  ",
    round(x$extent/10, 1), " km x ", round(x$extent/10, 1),
    " km\n  stratification: ", her,
    "\n  total abundance: ", sum(x$abundance), "\n", sep="")
  if (x$initial_location) {
    cat("  no events, duration: ", x$duration, " min\n", sep="")
  } else {
    cat("  ", ifelse(length(x$mixture) > 1, "mixture, ", ""),
      "duration: ", x$duration, " min\n", sep="")
  }
  invisible(x)
}
