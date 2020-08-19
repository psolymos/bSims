plot.bsims_detections <-
function(x,
event_type=NULL, tlim=NULL,
pch_nest, col_nest, cex_nest,
pch_vocal, col_vocal, cex_vocal,
lty_move, col_move, lwd_move,
lty_det_vocal, col_det_vocal, lwd_det_vocal,
lty_det_move, col_det_move, lwd_det_move,
condition="event1", ...) {

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
  if (missing(lty_det_vocal))
    lty_det_vocal <- .bsims_theme$lty_det_vocal
  if (missing(col_det_vocal))
    col_det_vocal <- .bsims_theme$col_det_vocal
  if (missing(lwd_det_vocal))
    lwd_det_vocal <- .bsims_theme$lwd_det_vocal
  if (missing(lty_det_move))
    lty_det_move <- .bsims_theme$lty_det_move
  if (missing(col_det_move))
    col_det_move <- .bsims_theme$col_det_move
  if (missing(lwd_det_move))
    lwd_det_move <- .bsims_theme$lwd_det_move

  if (is.null(event_type))
    event_type <- x$event_type
  op <- par(xpd = TRUE)
  on.exit(par(op))
  xx <- x
  class(xx) <- c("bsim", "bsims_events")
  plot(xx, tlim=tlim,
    pch_nest=pch_nest, col_nest=col_nest, cex_nest=cex_nest,
    pch_vocal=pch_vocal, col_vocal=col_vocal, cex_vocal=cex_vocal,
    lty_move=lty_move, col_move=col_move, lwd_move=lwd_move, ...)
  if (!is.na(lty_det_vocal))
    lines(x, event_type="vocal", tlim=tlim,
      col=col_det_vocal, lty=lty_det_vocal, lwd=lwd_det_vocal,
      condition=condition, ...)
  if (!is.na(lty_det_move))
    lines(x, event_type="move", tlim=tlim,
      col=col_det_move, lty=lty_det_move, lwd=lwd_det_move,
      condition=condition, ...)

  invisible(x)
}
