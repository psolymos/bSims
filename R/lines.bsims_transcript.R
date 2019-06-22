lines.bsims_transcript <-
function(x, event_type=c("vocal", "move", "both"), tlim=NULL, ...) {
  z <- get_detections(x,
    condition=x$conditio, event_type=event_type, tlim=tlim)
  xy <- z[,c("x", "y")]
  for (i in seq_len(nrow(z))) {
    lines(c(x$xy[1L], xy[i,1L]), c(x$xy[2L], xy[i,2L]), ...)
  }
  invisible(x)
}
