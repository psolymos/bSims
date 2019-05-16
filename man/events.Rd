\name{events}
\alias{events}
\alias{timetoevent}
\title{
Event time generator
}
\description{
\code{timetoevent} turns exponential wait times to time-to-event data
within a desired duration handling ~0 and infinite rates in a robust manner,
\code{events} generates event times based on exponential time-to-event
distribution.
}
\usage{
timetoevent(rate, duration)
events(vocal_rate = 1, move_rate = 1, duration = 10,
  movement = 0, avoid = c(0, 0))
}
\arguments{
  \item{vocal_rate}{
vocal rate for exponential distribution.
}
  \item{move_rate}{
movement rate for exponential distribution.
}
  \item{duration}{
total duration to consider.
}
  \item{movement}{
standard deviation for a bivariate Normal kernel to simulate movement from.
}
  \item{avoid}{
range along the x axis to avoid with respect to locations.
}
  \item{rate}{
rate for exponential distribution.
}
}
\value{
An events object data frame with coordinates (x, y; centered at 0 that is nest
location), event times (t) and indicator for vocal events (v).
}
\author{
Peter Solymos
}
\examples{
timetoevent(0, 10)
timetoevent(Inf, 10)

rr <- 1
tt <- timetoevent(rr, 10)
op <- par(mfrow=c(1,2))
plot(ecdf(tt))
curve(1-exp(-rr*x), add=TRUE, col=2)

plot(stepfun(sort(tt), 0:length(tt)/length(tt)), ylab="F(x)")
curve(1-exp(-rr*x), add=TRUE, col=2)
par(op)

e <- events(movement=1, duration=60)
plot(e[,1:2], col=e$v+1, asp=1)
abline(h=0, v=0, lty=2)
}
\keyword{ datagen }
\keyword{ distribution }
