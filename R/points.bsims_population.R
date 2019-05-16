points.bsims_population <-
function(x, ...) {
  points(x$nests[,c("x", "y")], ...)
  invisible(x)
}
