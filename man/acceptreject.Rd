\name{acceptreject}
\alias{acceptreject}
\title{
Spatial point process simulator
}
\description{
Spatial point process simulator based on
accept/reject algorithm.
}
\usage{
acceptreject(n, f = NULL, x0 = 0, x1 = 1, y0 = 0, y1 = 1,
  m = 0, maxit = 100, fail = FALSE)
}
\arguments{
  \item{n}{
number of points to generate.
}
  \item{f}{
a function returning probability (value between 0 and 1) given distance as
the first and only argument. The function generates
spatially uniform Poisson point process (complete spatial randomness)
when \code{NULL}.
}
  \item{x0, x1, y0, y1}{
x and y ranges (bounding box).
}
  \item{m}{
margin width for avoiding edge effects.
}
  \item{maxit}{
maximum number of iterations per point to try if no acceptance happens.
}
  \item{fail}{
logical, what to do when there is a problem.
\code{TRUE} gives error, the default \code{FALSE} gives only a warning.
}
}
\value{
A matrix with \code{n} rows and 2 columns for x and y coordinates.
}
\author{
Peter Solymos
}
\examples{
## complete spatial randomness
plot(acceptreject(100), asp=1)

## more systematic
distance <- seq(0,1,0.01)
f <- function(d)
  (1-exp(-d^2/0.1^2) + dlnorm(d, 0.2)/dlnorm(exp(0.2-1),0.2)) / 2
op <- par(mfrow = c(1, 2))
plot(distance, f(distance), type="l")
plot(acceptreject(100, f, m=1), asp=1)
par(op)

## more clustered
f <- function(d)
  exp(-d^2/0.1^2) + 0.5*(1-exp(-d^2/0.4^2))
op <- par(mfrow = c(1, 2))
plot(distance, f(distance), type="l")
plot(acceptreject(100, f, m=1), asp=1)
par(op)
}
\keyword{ datagen }
\keyword{ distribution }
\keyword{ spatial }
