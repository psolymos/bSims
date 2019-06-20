plot.bsims_detections <-
function(x,
event_type=c("vocal", "move", "both"), tlim=NULL,
pch_nest=3, col_nest="darkgreen", cex_nest=1,
pch_vocal=21, col_vocal="blue", cex_vocal=0.5,
lty_move=1, col_move="orange", lwd_move=1,
lty_det=1, col_det="black", lwd_det=1,
...) {
  op <- par(xpd = TRUE)
  on.exit(par(op))
  xx <- x
  class(xx) <- c("bsim", "bsims_events")
  plot(xx, tlim=tlim,
    pch_nest=pch_nest, col_nest=col_nest, cex_nest=cex_nest,
    pch_vocal=pch_vocal, col_vocal=col_vocal, cex_vocal=cex_vocal,
    lty_move=lty_move, col_move=col_move, lwd_move=lwd_move, ...)
  if (!is.na(lty_det))
    lines(x, event_type=event_type, tlim=tlim, col=col_det, lty=lty_det, ...)
  invisible(x)
}
