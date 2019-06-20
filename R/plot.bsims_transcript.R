plot.bsims_transcript <-
function(x,
pch_nest=3, col_nest="darkgreen", cex_nest=1,
pch_vocal=21, col_vocal="blue", cex_vocal=0.5,
lty_move=1, col_move="orange", lwd_move=1,
lty_det=1, col_det="black", lwd_det=1,
show_tint=TRUE, show_rint=TRUE,
col_tint="red", col_rint="red",
...) {
  op <- par(xpd = TRUE)
  on.exit(par(op))
  xx <- x
  class(xx) <- c("bsim", "bsims_detections")
  plot(xx,
    tlim=c(0, max(x$tint)),
    event_type=x$event_type,
    pch_nest=pch_nest, col_nest=col_nest, cex_nest=cex_nest,
    pch_vocal=pch_vocal, col_vocal=col_vocal, cex_vocal=cex_vocal,
    lty_move=lty_move, col_move=col_move, lwd_move=lwd_move, ...)
  if (show_rint) {
    col_rint <- col2hex(col_rint)
    rr <- x$rint
    rr <- rr[is.finite(rr)]
    if (length(rr) > 0) {
      if (any(is.infinite(x$rint))) {
        polygon(0.5*x$extent*c(-1,-1,1,1), 0.5*x$extent*c(-1,1,1,-1),
          border=NA, col=paste0(col_rint, "33"))
      } else {
        .draw_ellipse(0, 0, max(rr), max(rr),
          border=NA, col=paste0(col_rint, "33"))
      }
      .draw_ellipse(rep(0, length(rr)), rep(0, length(rr)), rr, rr,
        border=col_rint)
    } else {
      polygon(0.5*x$extent*c(-1,-1,1,1), 0.5*x$extent*c(-1,1,1,-1),
        border=NA, col=paste0(col_rint, "33"))
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

