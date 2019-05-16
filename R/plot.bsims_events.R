plot.bsims_events <-
function(x, tlim=NULL,
pch_nest=3, col_nest="darkgreen", cex_nest=1,
pch_vocal=21, col_vocal="blue", cex_vocal=0.5,
lty_move=1, col_move="orange", lwd_move=1,
...) {
  op <- par(xpd = TRUE)
  on.exit(par(op))
  xx <- x
  class(xx) <- c("bsim", "bsims_population")
  plot(xx, pch_nest=pch_nest, col_nest=col_nest, cex_nest=cex_nest, ...)
  if (!is.na(lty_move))
    lines(x, tlim=tlim, col=col_move, lty=lty_move, lwd=lwd_move, ...)
  if (!is.na(pch_vocal))
    points(x, vocal_only=TRUE, tlim=tlim,
      col=col_vocal, pch=pch_vocal, cex=cex_vocal, ...)
  invisible(x)
}
