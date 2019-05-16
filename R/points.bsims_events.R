points.bsims_events <-
function(x, vocal_only=TRUE, tlim=NULL, ...) {
  points(get_events(x, vocal_only, tlim), ...)
  invisible(x)
}
