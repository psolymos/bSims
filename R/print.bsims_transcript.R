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
    "\n  total abundance: ", sum(x$abundance),
    "\n  ", ifelse(length(x$mixture) > 1, "mixture with ", ""),
    "total duration: ", x$duration, "\n  detected: ", ndet,
    ifelse(x$vocal_only, " heard", " seen/heard"),
    "\n  ", ifelse(x$first_only, "1st", "all"),
    " inds. [", paste0(gsub("min", "", levels(x$det$tint)), collapse=", "),
    " min] [", paste0(gsub("m", "", levels(x$det$rint)), collapse=", "), " m]\n", sep="")
  invisible(x)
}
