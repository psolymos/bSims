\name{plot.bsims_landscape}
\alias{plot.bsims_landscape}
\alias{plot.bsims_population}
\alias{plot.bsims_events}
\alias{plot.bsims_detections}
\alias{plot.bsims_transcript}
\alias{lines.bsims_detections}
\alias{lines.bsims_events}
\alias{lines.bsims_transcript}
\alias{points.bsims_population}
\alias{points.bsims_events}
\alias{points.bsims_detections}
\alias{col2hex}
\alias{plot.bsims_events_table}
\alias{plot.bsims_detections_table}
\title{
Plot methods
}
\description{
Plot methods for different bSims objects.
}
\usage{
\method{plot}{bsims_landscape}(x,
  col_H, col_E, col_R,
  xlim = NULL, ylim = NULL, ...)

\method{plot}{bsims_population}(x,
  pch_nest, col_nest, cex_nest, ...)

\method{plot}{bsims_events}(x,
  event_type=c("vocal", "move", "both"), tlim = NULL,
  pch_nest, col_nest, cex_nest,
  pch_vocal, col_vocal, cex_vocal,
  lty_move, col_move, lwd_move, ...)

\method{plot}{bsims_detections}(x,
  event_type=NULL, tlim = NULL,
  pch_nest, col_nest, cex_nest,
  pch_vocal, col_vocal, cex_vocal,
  lty_move, col_move, lwd_move,
  lty_det, col_det, lwd_det,
  condition = "event1", ...)

\method{plot}{bsims_transcript}(x,
  pch_nest, col_nest, cex_nest,
  pch_vocal, col_vocal, cex_vocal,
  lty_move, col_move, lwd_move,
  lty_det, col_det, lwd_det,
  show_tint=TRUE, show_rint=TRUE,
  col_tint, col_rint, ...)

\method{lines}{bsims_events}(x, tlim = NULL, ...)
\method{lines}{bsims_detections}(x,
  event_type=NULL, tlim=NULL, condition="event1", ...)
\method{lines}{bsims_transcript}(x,
  event_type=NULL, tlim=NULL, ...)

\method{points}{bsims_population}(x, ...)
\method{points}{bsims_events}(x,
  event_type=c("vocal", "move", "both"), tlim = NULL, ...)
\method{points}{bsims_detections}(x,
  event_type=NULL, tlim=NULL, condition="event1", ...)

col2hex(col, alpha = FALSE)

\method{plot}{bsims_events_table}(x,
  xlab, ylab, xlim, ylim, ...)
\method{plot}{bsims_detections_table}(x,
  type=c("time", "distance"), xlab, ylab, xlim, ylim, ...)
}
\arguments{
  \item{x}{
simulation object.
}
  \item{col}{
color values.
}
  \item{col_H, col_E, col_R}{
color values for the Habitat, Edge, and Road strata.
}
  \item{event_type}{
type of events to access.
The value is inferred from the input object when \code{NULL}.
}
  \item{xlim, ylim, tlim}{
x, y, time intervals.
}
  \item{xlab, ylab}{
x and y axis labels.
}
  \item{pch_nest, col_nest, cex_nest}{
visual characteristics of nest locations.
}
  \item{pch_vocal, col_vocal, cex_vocal}{
visual characteristics of vocalization events.
}
  \item{lty_move, col_move, lwd_move}{
visual characteristics of movement events.
}
  \item{lty_det, col_det, lwd_det}{
visual characteristics of detection events.
}
  \item{alpha}{
alpha channel for colors.
}
  \item{show_tint, show_rint}{
whether time and distance intervals should be displayed.
}
  \item{col_tint, col_rint}{
colors for time and distance intervals.
}
  \item{condition}{
conditioning type to define availability for each individual,
see \code{\link{bsims_detect}}.
}
  \item{type}{
what the x axis should be: time or distance.
}
  \item{\dots}{
other graphical arguments.
}
}
\details{
The main plotting functions use a theme defined in
the option \code{getOption("bsims_theme")}.
Overriding these default settings allows customization.
}
\value{
These plotting functions are called for their side effects
and silently return the input object.

\code{col2hex} is modeled after \code{\link{col2rgb}}
and returns a character vector giving hexadecimal color codes with
or without alpha channel values.
}
\author{
Peter Solymos
}
\seealso{
\code{\link{bsims_init}}, \code{\link{col2rgb}}
}
\examples{
b <- bsims_all(road=1, edge=2, move_rate=1, movement=0.2)$new()
o <- getOption("bsims_theme")
str(o)
n <- o
n$col_H <- "gold"
n$col_E <- "magenta"
n$col_R <- "black"
op <- par(mfrow=c(1, 2))
plot(b)
options("bsims_theme" = n) # apply new theme
plot(b)
par(op)
options("bsims_theme" = o) # reset old theme

col2hex(c(blu = "royalblue", reddish = "tomato"), alpha = FALSE)
col2hex(c(blu = "royalblue", reddish = "tomato"), alpha = TRUE)
}
\keyword{ hplot }
\keyword{ aplot }
