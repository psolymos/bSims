lines.bsims_detections <-
function(x, event_type=NULL, tlim=NULL, condition="event1", ...) {
  if (is.null(event_type))
    event_type <- x$event_type
  z <- .get_detections(x,
    condition=condition, event_type=event_type, tlim=tlim)
  xy <- z[,c("x", "y")]
  for (i in seq_len(nrow(z))) {
    lines(c(x$xy[1L], xy[i,1L]), c(x$xy[2L], xy[i,2L]), ...)
  }
  invisible(x)
}
