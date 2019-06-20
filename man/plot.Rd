\name{plot.bsims_landscape}
\alias{plot.bsims_landscape}
\alias{plot.bsims_population}
\alias{plot.bsims_events}
\alias{plot.bsims_detections}
\alias{plot.bsims_transcript}
\alias{lines.bsims_detections}
\alias{lines.bsims_events}
\alias{points.bsims_population}
\alias{points.bsims_events}
\alias{points.bsims_detections}
\title{
Plot methods for populations
}
\description{
Plotting functionalities.
}
\usage{
\method{plot}{bsims_landscape}(x,
  col = c("darkolivegreen1", "burlywood1", "lightgrey"),
  xlim = NULL, ylim = NULL, ...)

\method{plot}{bsims_population}(x, pch_nest=3,
  col_nest="darkgreen", cex_nest=1, ...)

\method{plot}{bsims_events}(x,
  event_type=c("vocal", "move", "both"), tlim = NULL,
  pch_nest = 3, col_nest = "darkgreen", cex_nest = 1,
  pch_vocal = 21, col_vocal = "blue", cex_vocal = 0.5,
  lty_move = 1, col_move = "orange", lwd_move = 1, ...)

\method{plot}{bsims_detections}(x,
  event_type=c("vocal", "move", "both"), tlim = NULL,
  pch_nest = 3, col_nest = "darkgreen", cex_nest = 1,
  pch_vocal = 21, col_vocal = "blue", cex_vocal = 0.5,
  lty_move = 1, col_move = "orange", lwd_move = 1,
  lty_det = 1, col_det = "black", lwd_det = 1, ...)

\method{plot}{bsims_transcript}(x,
  event_type=c("vocal", "move", "both"), tlim = NULL,
  pch_nest = 3, col_nest = "darkgreen", cex_nest = 1,
  pch_vocal = 21, col_vocal = "blue", cex_vocal = 0.5,
  lty_move = 1, col_move = "orange", lwd_move = 1,
  lty_det = 1, col_det = "black", lwd_det = 1, ...)

\method{lines}{bsims_events}(x, tlim = NULL, ...)
\method{lines}{bsims_detections}(x,
  event_type=c("vocal", "move", "both"), tlim=NULL, ...)

\method{points}{bsims_population}(x, ...)
\method{points}{bsims_events}(x,
  event_type=c("vocal", "move", "both"), tlim = NULL, ...)
\method{points}{bsims_detections}(x,
  event_type=c("vocal", "move", "both"), tlim=NULL, ...)
}
\arguments{
  \item{x}{
simulation object.
}
  \item{col}{
stratum colors.
}
  \item{event_type}{
type of events to access.
}
  \item{xlim, ylim, tlim}{
x, y, time intervals.
}
  \item{pch_nest, col_nest, cex_nest}{
nest.
}
  \item{pch_vocal, col_vocal, cex_vocal}{
vocal.
}
  \item{lty_move, col_move, lwd_move}{
movement.
}
  \item{lty_det, col_det, lwd_det}{
detection.
}
  \item{\dots}{
other graphical arguments.
}
}
\author{
Peter Solymos
}
\seealso{
\code{\link{bsims_init}}
}
\keyword{ hplot }
\keyword{ aplot }
