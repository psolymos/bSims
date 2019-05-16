plot.bsims_population <-
function(x, pch_nest=3,
col_nest="darkgreen", cex_nest=1,
...) {
  op <- par(xpd = TRUE)
  on.exit(par(op))
  xx <- x
  class(xx) <- c("bsim", "bsims_landscape")
  plot(xx, ...)
  if (!is.na(pch_nest))
    points(x, pch=pch_nest, col=col_nest, cex=cex_nest, ...)
  invisible(x)
}
