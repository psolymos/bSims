plot.bsims_events <-
function(x, event_type=c("vocal", "move", "both"), tlim=NULL,
pch_nest, col_nest, cex_nest,
pch_vocal, col_vocal, cex_vocal,
lty_move, col_move, lwd_move,
...) {

  .bsims_theme <- getOption("bsims_theme")
  if (missing(pch_nest))
    pch_nest <- .bsims_theme$pch_nest
  if (missing(col_nest))
    col_nest <- .bsims_theme$col_nest
  if (missing(cex_nest))
    cex_nest <- .bsims_theme$cex_nest
  if (missing(pch_vocal))
    pch_vocal <- .bsims_theme$pch_vocal
  if (missing(col_vocal))
    col_vocal <- .bsims_theme$col_vocal
  if (missing(cex_vocal))
    cex_vocal <- .bsims_theme$cex_vocal
  if (missing(lty_move))
    lty_move <- .bsims_theme$lty_move
  if (missing(col_move))
    col_move <- .bsims_theme$col_move
  if (missing(lwd_move))
    lwd_move <- .bsims_theme$lwd_move

  op <- par(xpd = TRUE)
  on.exit(par(op))
  xx <- x
  class(xx) <- c("bsim", "bsims_population")
  plot(xx, pch_nest=pch_nest, col_nest=col_nest, cex_nest=cex_nest, ...)
  if (!is.na(lty_move))
    lines(x, tlim=tlim, col=col_move, lty=lty_move, lwd=lwd_move, ...)
  if (!is.na(pch_vocal))
    points(x, event_type=event_type, tlim=tlim,
      col=col_vocal, pch=pch_vocal, cex=cex_vocal, ...)
  invisible(x)
}
