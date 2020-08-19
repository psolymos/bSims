plot.bsims_transcript <-
function(x,
pch_nest, col_nest, cex_nest,
pch_vocal, col_vocal, cex_vocal,
lty_move, col_move, lwd_move,
lty_det_vocal, col_det_vocal, lwd_det_vocal,
lty_det_move, col_det_move, lwd_det_move,
show_tint=TRUE, show_rint=TRUE,
col_tint, col_rint,
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
  if (missing(col_tint))
    col_tint <- .bsims_theme$col_tint
  if (missing(col_rint))
    col_rint <- .bsims_theme$col_rint

  op <- par(xpd = TRUE)
  on.exit(par(op))
  xx <- x
  class(xx) <- c("bsim", "bsims_events")
  plot(xx,
    tlim=c(0, max(x$tint)),
    event_type=x$event_type,
    pch_nest=pch_nest, col_nest=col_nest, cex_nest=cex_nest,
    pch_vocal=pch_vocal, col_vocal=col_vocal, cex_vocal=cex_vocal,
    lty_move=lty_move, col_move=col_move, lwd_move=lwd_move, ...)
  #lines(x, event_type=x$event_type, col=col_det, lwd=lwd_det)
  if (!is.na(lty_det_vocal))
    lines(x, event_type="vocal", tlim=c(0, max(x$tint)),
      col=col_det_vocal, lty=lty_det_vocal, lwd=lwd_det_vocal)
  if (!is.na(lty_det_move))
    lines(x, event_type="move", tlim=c(0, max(x$tint)),
      col=col_det_move, lty=lty_det_move, lwd=lwd_det_move)
  if (show_rint) {
    col_rint <- col2hex(col_rint)
    rr <- x$rint
    rr <- rr[is.finite(rr)]
    if (any(is.infinite(x$rint))) {
      polygon(0.5*x$extent*c(-1,-1,1,1), 0.5*x$extent*c(-1,1,1,-1),
        border=NA, col=paste0(col_rint, "33"))
    } else {
      .draw_ellipse(0, 0, max(rr), max(rr),
        border=NA, col=paste0(col_rint, "33"))
    }
    if (length(rr) > 0) {
      .draw_ellipse(rep(0, length(rr)), rep(0, length(rr)), rr, rr,
        border=col_rint)
    }
  }
  if (show_tint) {
    col_tint <- col2hex(col_tint)
    tt <- x$tint
    tt <- x$extent * x$tint / x$duration
    tt <- c(0, tt) * 0.8 - (x$extent * 0.4)
    if (max(x$tint) < x$duration)
      polygon(x$extent*0.4*c(-1,-1,1,1), x$extent*c(0.4, 0.45, 0.45, 0.4),
        border=col_tint, col=NA, lty=2)
    polygon(x$extent*0.4*c(-1,-1,1,1), x$extent*c(0.4, 0.45, 0.45, 0.4),
      border=col_tint, col=NA, lty=2)
    for (i in 2:length(tt)) {
      polygon(tt[c(i-1, i-1, i, i)],
        x$extent*c(0.4, 0.45, 0.45, 0.4),
        border=col_tint, col=paste0(col_tint, "33"), lty=1)
    }
  }
  invisible(x)
}

