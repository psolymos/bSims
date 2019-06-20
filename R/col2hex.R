col2hex <- function(col, alpha = FALSE) {
  rgb <- col2rgb(col, alpha)
  if (alpha) {
    apply(rgb, 2, function(z) rgb(z[1], z[2], z[3], z[4], maxColorValue=255))
  } else {
    apply(rgb, 2, function(z) rgb(z[1], z[2], z[3], maxColorValue=255))
  }
}
