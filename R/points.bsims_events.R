points.bsims_events <-
function(x, event_type=c("vocal", "move", "both"), tlim=NULL, ...) {
  points(.get_events(x, event_type, tlim), ...)
  invisible(x)
}
