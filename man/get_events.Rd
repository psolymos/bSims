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
get_detections(x, first_only = TRUE, tlim = NULL)
}
\arguments{
  \item{x}{
simulation object.
}
  \item{event_type}{
type of events to access.
}
  \item{first_only}{
access 1st detctions only.
}
  \item{tlim}{
time intervals.
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
