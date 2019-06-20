points.bsims_detections <-
function(x, event_type=c("vocal", "move", "both"), tlim=NULL, ...) {
  z <- get_detections(x,
    condition="event1", event_type=event_type, tlim=tlim)
  points(z[,c("x", "y")], ...)
  invisible(x)
}
