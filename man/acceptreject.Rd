\name{acceptreject}
\alias{acceptreject}
\title{
Spatial point process simulator.
}
\description{
\code{acceptreject}
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
the first and only argument, Poisson point process (complete spatial randomness)
if \code{NULL}.
}
  \item{x0, x1, y0, y1}{
x and y ranges (bounding box).
}
  \item{m}{
margin width for edge effect.
}
  \item{maxit}{
maximum number of iterations.
}
  \item{fail}{
logical, what to do when there is a problem (error or warning).
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

## close to systematic
distance <- seq(0,1,0.01)
f <- function(d) 1-exp(-d^2/0.1^2)
plot(distance, f(distance), type="l")
plot(acceptreject(100, f, m=1), asp=1)

## bimodal/clustered
f <- function(d) pmax(ifelse(d < 0.1, 1, 0), 0.5*(1-exp(-d^2/0.5^2)))
plot(distance, f(distance), type="l")
plot(acceptreject(100, f, m=1), asp=1)
}
\keyword{ datagen }
\keyword{ distribution }
\keyword{ spatial }
