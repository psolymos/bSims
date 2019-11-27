plot.bsims_population <-
function(x, pch_nest, col_nest, cex_nest, ...) {

  .bsims_theme <- getOption("bsims_theme")
  if (missing(pch_nest))
    pch_nest <- .bsims_theme$pch_nest
  if (missing(col_nest))
    col_nest <- .bsims_theme$col_nest
  if (missing(cex_nest))
    cex_nest <- .bsims_theme$cex_nest

  op <- par(xpd = TRUE)
  on.exit(par(op))
  xx <- x
  class(xx) <- c("bsim", "bsims_landscape")
  plot(xx, ...)
  if (!is.na(pch_nest))
    points(x, pch=pch_nest, col=col_nest, cex=cex_nest, ...)
  invisible(x)
}
