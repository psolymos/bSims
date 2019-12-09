\name{get_nests}
\alias{get_nests}
\alias{get_events}
\alias{get_detections}
\alias{get_abundance}
\alias{get_density}
\alias{get_nests.bsims_population}
\alias{get_abundance.bsims_population}
\alias{get_density.bsims_population}
\alias{get_events.bsims_events}
\alias{get_detections.bsims_detections}
\title{
Access nests, events, detections, and totals
}
\description{
Access nests, events, detections,
abundance, and density from simulation objects.
}
\usage{
get_nests(x, ...)
\method{get_nests}{bsims_population}(x, ...)

get_events(x, ...)
\method{get_events}{bsims_events}(x, ...)

get_detections(x, ...)
\method{get_detections}{bsims_detections}(x, ...)

get_abundance(x, ...)
\method{get_abundance}{bsims_population}(x, ...)

get_density(x, ...)
\method{get_density}{bsims_population}(x, ...)
}
\arguments{
  \item{x}{
simulation object.
}
  \item{\dots}{
other arguments passed to internal functions.
}
}
\details{
\code{get_nests} extracts the next locations.

\code{get_events} extracts the events.

\code{get_detections} extracts the detections.

\code{get_abundance} gets total abundance (N),
\code{get_density} gets average density (abundance/area: N/A).
}
\value{
\code{get_abundance} and \code{get_density}
returns a non-negative numeric value.

\code{get_nests} returns a data frame with the following columns:
\code{i} individual identifier,
\code{s} spatial stratum (H: habitat, E: edge, R: road)
\code{x} and \code{y} are coordinates of the nest locations,
\code{g} is behavioural (mixture) group or \code{NA}.

\code{get_events} returns a data frame with the following columns:
\code{x} and \code{y} are locations of the individual at the time of the event,
\code{t} time of the event within the duration interval,
\code{v} indicator variable for vocal (1) vs. movement (0) event,
\code{i} individual identifier.

\code{get_detections} returns a data frame with the following columns:
\code{x} and \code{y} are locations of the individual at the time of the event,
\code{t} time of the event within the duration interval,
\code{v} indicator variable for vocal (1) vs. movement (0) event,
\code{d} distance from observer when detected (otherwise \code{NA}).
\code{i} individual identifier,
\code{j} perceived individual identifier.
}
\author{
Peter Solymos
}
\seealso{
\code{\link{bsims_init}}
}
\examples{
phi <- 0.5                 # singing rate
tau <- 1:3                 # EDR by strata
dur <- 10                  # simulation duration
tbr <- c(3, 5, 10)         # time intervals
rbr <- c(0.5, 1, 1.5, Inf) # counting radii

l <- bsims_init(10, 0.5, 1)# landscape
p <- bsims_populate(l, 1)  # population
e <- bsims_animate(p,      # events
  vocal_rate=phi, duration=dur)
d <- bsims_detect(e,       # detections
  tau=tau)
x <- bsims_transcribe(d,   # transcription
  tint=tbr, rint=rbr)

## next locations
head(get_nests(p))
head(get_nests(e))
head(get_nests(d))
head(get_nests(x))

## abundance
get_abundance(p)
get_abundance(e)
get_abundance(d)
get_abundance(x)

## density
get_density(p)
get_density(e)
get_density(d)
get_density(x)

## events
head(get_events(e))
head(get_events(d))
head(get_events(x))

## detections
head(get_detections(d))
head(get_detections(x))
}
\keyword{ manip }
