\name{get_events}
\alias{get_events}
\alias{get_detections}
\title{
Access events or detections from simulation object
}
\description{
Access events or detections from simulation object.
}
\usage{
get_events(x, event_type=c("vocal", "move", "both"), tlim = NULL)

get_detections(x, condition = c("event1", "det1", "alldet"),
  event_type = c("vocal", "move", "both"), tlim = NULL)
}
\arguments{
  \item{x}{
simulation object.
}
  \item{event_type}{
type of events to access.
}
  \item{condition}{
conditioning type to define availability for each individual:
\code{"event1"}: the 1st event;
\code{"det1"}: the 1st detection;
\code{"alldet"}: all detections (count same individual multiple timmes).
}
  \item{tlim}{
time intervals treated as [\code{min(tlim)}, \code{max(tlim)}).
}
}
\value{
Events or detections.
}
\author{
Peter Solymos
}
\seealso{
\code{\link{bsims_animate}}, \code{\link{bsims_detect}}
}
\keyword{ manip }
