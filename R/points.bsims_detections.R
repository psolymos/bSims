points.bsims_detections <-
function(x, event_type=NULL, tlim=NULL, condition="event1", ...) {
  if (is.null(event_type))
    event_type <- x$event_type
  z <- .get_detections(x,
    condition=condition, event_type=event_type, tlim=tlim)
  points(z[,c("x", "y")], ...)
  invisible(x)
}
