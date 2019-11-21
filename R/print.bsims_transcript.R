print.bsims_transcript <-
function(x, ...) {
  A <- diff(x$strata) * diff(range(x$strata))
  A <- c(h=A[1]+A[5], e=A[2]+A[4], r=A[3])
  names(A) <- c("H", "E", "R")
  her <- paste0(
    ifelse(A[1] > 0, "H", ""),
    ifelse(A[2] > 0, "E", ""),
    ifelse(A[3] > 0, "R", ""), collapse="")
  ndet <- if (sum(x$abundance) == 0)
    0 else sum(sapply(x$events, function(z) any(!is.na(z$d))))
  cat("bSims transcript\n  ",
    round(x$extent/10, 1), " km x ", round(x$extent/10, 1),
    " km\n  stratification: ", her,
    "\n  total abundance: ", sum(x$abundance), "\n", sep="")
  if (x$initial_location) {
    cat("  no events, duration: ", x$duration, " min\n", sep="")
  } else {
    cat("  ", ifelse(length(x$mixture) > 1, "mixture, ", ""),
      "duration: ", x$duration, " min\n", sep="")
  }
  cat("  detected: ", ndet,
    switch(x$event_type, "vocal"=" heard",
      "move"=" seen", "both"=" seen/heard"), "\n", sep="")
  cat("  ", switch(x$condition, "event1"="1st event detected",
    "det1"="1st detection", "alldet"="all detections"),
    " by breaks:\n    [",
    paste0(round(c(0, x$tint), 2), collapse=", "), " min]\n    [",
    paste0(round(100 * c(0, x$rint)), collapse=", "), " m]\n", sep="")
  invisible(x)
}
