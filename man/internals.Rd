\name{internals}
\alias{.bsims_all}
\alias{.get_events}
\alias{.get_detections}
\title{
Internal functions
}
\description{
Internal functions are not intended to be used by the user.
}
\usage{
.get_events(x, event_type=c("vocal", "move", "both"), tlim = NULL)

.get_detections(x, condition = c("event1", "det1", "alldet"),
  event_type = c("vocal", "move", "both"), tlim = NULL,
  perception = NULL)

.bsims_all(Settings)
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
\code{"event1"}: the 1st event (detected or not);
\code{"det1"}: the 1st detection;
\code{"alldet"}: all detections (counting the same individual multiple times).
}
  \item{tlim}{
time intervals treated as [\code{min(tlim)}, \code{max(tlim)}).
}
  \item{perception}{
perceived number of individuals relative to the actual number of individuals.
A non-negative number (<1 values lead to under counting,
>1 values lead to over counting),
or \code{NULL} (observer correctly identifies all individuals).
}
  \item{Settings}{
a list of arguments.
}
}
\keyword{ internal }
