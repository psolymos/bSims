\name{dist_fun2}
\alias{dist_fun2}
\title{
Half-Normal distance function with segmented attenuation
}
\description{
Half-Normal distance function with segmented attenuation
depending in 3 strata (habitat, edge, road)
}
\usage{
dist_fun2(d, tau, dist_fun, b = numeric(0))
}
\arguments{
  \item{d}{
distance from observer.
}
  \item{tau}{
a parameter passed to the the distance function, need to be in 1-3 in length
referring to distance function parameters in the 3 strata.
}
  \item{dist_fun}{
distance functon taking two arguments: for distance, and \code{tau}, see examples.
}
  \item{b}{
distance breakpoints, must be \code{length(tau)-1} in length.
}
}
\value{
Probability of detection given distance, strata specific parameters and breakpoints.
}
\author{
Peter Solymos
}
\examples{
tau <- c(1,2,3,2,1)
d <- 0:400/100
dist_fun <- function(d, tau) exp(-d^2/tau^2) # half normal
#dist_fun <- function(d, tau) exp(-d/tau^2) # exponential
plot(d, dist_fun2(d, tau[1], dist_fun), type="l")
lines(d, dist_fun2(d, tau[2], dist_fun))
lines(d, dist_fun2(d, tau[3], dist_fun))
b <- c(0.5, 1, 1.5, 2) # points at the HER boundaries
abline(v=b)
lines(d, dist_fun2(d, tau, dist_fun, b), col=2, lwd=3)
}
\keyword{ math }
