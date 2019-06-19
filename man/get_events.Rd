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
get_events(x, vocal_only = TRUE, tlim = NULL)
get_detections(x, first_only = TRUE, tlim = NULL)
}
\arguments{
  \item{x}{
simulation object.
}
  \item{vocal_only}{
access vocalization only.
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
