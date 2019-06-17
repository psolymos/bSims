\name{bsims_init}
\alias{bSims-package}
\alias{bSims}
\alias{bsims_init}
\alias{bsims_populate}
\alias{bsims_animate}
\alias{bsims_detect}
\alias{bsims_transcribe}
\alias{print.bsims_landscape}
\alias{print.bsims_population}
\alias{print.bsims_events}
\alias{print.bsims_detections}
\alias{print.bsims_transcript}
\title{
bSims functions
}
\description{
Functions to initialize, populate, animate, detect, and transcribe
simulated birds in a point count.
}
\usage{
bsims_init(extent = 10, road = 0, edge = 0, offset = 0)

bsims_populate(x, density = 1, abund_fun = NULL, xy_fun = NULL,
  margin = 0, maxit = 100, fail = FALSE, ...)

bsims_animate(x, vocal_rate = 1, move_rate = 1, duration = 10,
  movement = 0, mixture = 1, avoid = c("none", "R", "ER"),
  initial_location=FALSE, ...)

bsims_detect(x, xy = c(0, 0), tau = 1, dist_fun = NULL, repel = 0,
  vocal_only = TRUE, ...)

bsims_transcribe(x, tint = NULL, rint = Inf,
  first_only = TRUE, error = 0, ...)

\method{print}{bsims_landscape}(x, ...)
\method{print}{bsims_population}(x, ...)
\method{print}{bsims_events}(x, ...)
\method{print}{bsims_detections}(x, ...)
\method{print}{bsims_transcript}(x, ...)
}
\arguments{
  \item{extent}{
extent of simulation area, an extent x extent square with (0,0) at the center.
}
  \item{road}{
half width of the road stratum (parallel to the y axis).
}
  \item{edge}{
width of edge, same width on both sides of the road stratum.
}
  \item{offset}{
offset to apply to road and edge strata relative to the center in the x direction.
}
  \item{x}{
a simulation object.
}
  \item{density}{
population density, D, recycled 3x for HER
}
  \item{abund_fun}{
function to simulate abundance, N ~ Pois(lambda), lambda=DA by default.
}
  \item{xy_fun}{
function to simulate nest locations, see \code{\link{acceptreject}}.
\code{NULL} is complete spatial randomness
}
  \item{margin, maxit, fail}{
arguments passed to \code{xy_fun}, see \code{\link{acceptreject}}.
}
  \item{vocal_rate, move_rate}{
Vocal and movement rates (see \code{\link{events}}).
Rate can be a single number, a vector matching length of mixture,
vector of length 3 with mixture=1 (corresponding to HER strata),
or a matrix of 3 x mixture (HER + groups).
}
  \item{movement, duration}{
arguments passed to \code{xy_fun}, see \code{\link{events}}.
}
  \item{mixture}{
finite mixture group proportions, can be a matrix referring
population segments.
}
  \item{avoid}{
strata to avoid by movement.
}
  \item{initial_location}{
logical, \code{move_rate} and \code{vocal_rate} silently ignored if \code{TRUE}
and nest ocation is provided as part of the events table,
this reders all individuals equally available for detection.
}
  \item{xy}{
position of the observer.
}
  \item{tau}{
parameter of the distance function.
}
  \item{dist_fun}{
distance function (1st argument is distance, second is tau).
}
  \item{repel}{
radius from observer within which events are invalidated.
}
  \item{vocal_only}{
should we detect visuals or just vocals.
}
  \item{tint}{
time interval break points.
}
  \item{rint}{
distance interval break points.
}
  \item{first_only}{
access 1st detctions only.
}
  \item{error}{
log scale standard deviation for distance estimation error,
see \code{\link{rlnorm2}}.
}
  \item{\dots}{
other arguments passed to underlying functions.
}
}
\details{
The functions are supposed to be called in sequence,
allowing to simulate multiple realities by keeping
preceding layers intact. Construction allows easy piping.
}
\value{
\code{bsims_init} returns a landscape object.

\code{bsims_populate} returns a population object.

\code{bsims_animate} returns an events object.

\code{bsims_detect} returns a detections object.

\code{bsims_transcribe} returns a transcript object.
}
\author{
Peter Solymos
}
\seealso{
\code{\link{acceptreject}}, \code{\link{events}},
\code{\link{get_events}}, \code{\link{get_detections}},
plotting functions \code{\link{plot.bsims_landscape}}.
}
\examples{
phi <- 0.5
tau <- 1:3
dur <- 10
rbr <- c(0.5, 1, 1.5, Inf)
tbr <- c(3, 5, 10)
l <- bsims_init(10, 0.5, 1)
p <- bsims_populate(l, 1)
a <- bsims_animate(p, vocal_rate=phi, duration=dur)
o <- bsims_detect(a, tau=tau)
x <- bsims_transcribe(o, tint=tbr, rint=rbr)
x
plot(x)
x$removal
x$visits
}
\keyword{ datagen }
