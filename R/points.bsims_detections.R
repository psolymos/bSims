points.bsims_detections <-
function(x, first_only=TRUE, ...) {
  points(get_detections(x, first_only)[,c("x", "y")], ...)
  invisible(x)
}
