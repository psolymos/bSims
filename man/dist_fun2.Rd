\name{dist_fun2}
\alias{dist_fun2}
\title{
Half-Normal distance function with segmented attenuation
}
\description{
Distance function with segmented attenuation
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
distance funciton taking two arguments: for distance, and \code{tau}, see examples.
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
d <- seq(0, 4, 0.01)
dist_fun <- function(d, tau) exp(-(d/tau)^2) # half normal
#dist_fun <- function(d, tau) exp(-d/tau) # exponential
#dist_fun <- function(d, tau) 1-exp(-(d/tau)^-2) # hazard rate

b <- c(0.5, 1, 1.5, 2) # points at the HER boundaries

op <- par(mfrow=c(2,1))
plot(d, dist_fun2(d, tau[1], dist_fun), type="n",
  ylab="P(detection)", xlab="Distance", axes=FALSE)
axis(1)
axis(2)
for (i in seq_len(length(b)+1)) {
  x1 <- c(0, b, 4)[i]
  x2 <- c(0, b, 4)[i+1]
  polygon(c(0, b, 4)[c(i, i, i+1, i+1)], c(0, 1, 1, 0),
    border=NA,
    col=c("darkolivegreen1", "burlywood1", "lightgrey",
    "burlywood1", "darkolivegreen1")[i])
}
lines(d, dist_fun2(d, tau[1], dist_fun))
lines(d, dist_fun2(d, tau[2], dist_fun))
lines(d, dist_fun2(d, tau[3], dist_fun))
lines(d, dist_fun2(d, tau, dist_fun, b), col=2, lwd=3)

plot(rev(d), dist_fun2(d, tau[1], dist_fun), type="n",
  ylab="P(detection)", xlab="Distance", axes=FALSE)
axis(1)
axis(2)
for (i in seq_len(length(b)+1)) {
  x1 <- c(0, b, 4)[i]
  x2 <- c(0, b, 4)[i+1]
  polygon(c(0, b, 4)[c(i, i, i+1, i+1)], c(0, 1, 1, 0),
    border=NA,
    col=c("darkolivegreen1", "burlywood1", "lightgrey",
    "burlywood1", "darkolivegreen1")[i])
}
lines(rev(d), dist_fun2(d, tau[1], dist_fun))
lines(rev(d), dist_fun2(d, tau[2], dist_fun))
lines(rev(d), dist_fun2(d, tau[3], dist_fun))
lines(rev(d), dist_fun2(d, tau, dist_fun, rev(4-b)), col=2, lwd=3)
par(op)
}
\keyword{ math }
