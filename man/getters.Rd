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

\code{get_total} total abundance or average density.
}
\value{
Events or detections.
}
\author{
Peter Solymos
}
\seealso{
\code{\link{bsims_init}}
}
\keyword{ manip }
