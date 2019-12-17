\name{dist_fun2}
\alias{dist_fun2}
\title{
Distance function with segmented attenuation
}
\description{
Distance function with segmented attenuation
crossing a number of boundaries of strata
with different attenuation characteristics
following results in Yip et al. (2017).
}
\usage{
dist_fun2(d, tau, dist_fun, breaks = numeric(0), ...)
}
\arguments{
  \item{d}{
distance from observer.
}
  \item{tau}{
a parameter passed to the the distance function.
Length of \code{tau} must equal \code{length(b) + 1}
referring to distance function parameters in the different strata
(a stratum is defined by an interval surrounded by 1 or 2 boundaries).
}
  \item{dist_fun}{
distance function taking two arguments: distance, and \code{tau}, see examples.
}
  \item{breaks}{
distance breakpoints, must be \code{length(tau) - 1} in length.
These breakpoints represent the boundaries between the intervals
characterized by homogeneous attenuation characteristics.
}
  \item{\dots}{
other arguments passed to \code{dist_fun}
}
}
\value{
Probability of detection given the distance, stratum specific parameters
and the arrangement of breakpoints.
}
\references{
Yip, D. A., Bayne, E. M., Solymos, P., Campbell, J.,
and Proppe, J. D., 2017.
Sound attenuation in forested and roadside environments:
implications for avian point count surveys.
\emph{Condor}, \bold{119}: 73--84.
<doi:10.1650/CONDOR-16-93.1>
}
\author{
Peter Solymos
}
\examples{
tau <- c(1, 2, 3, 2, 1)
d <- seq(0, 4, 0.01)
dist_fun <- function(d, tau) exp(-(d/tau)^2) # half normal
#dist_fun <- function(d, tau) exp(-d/tau) # exponential
#dist_fun <- function(d, tau) 1-exp(-(d/tau)^-2) # hazard rate

b <- c(0.5, 1, 1.5, 2) #  boundaries

op <- par(mfrow=c(2, 1))
plot(d, dist_fun2(d, tau[1], dist_fun), type="n",
  ylab="P(detection)", xlab="Distance", axes=FALSE,
  main="Sound travels from left to right")
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
  ylab="P(detection)", xlab="Distance", axes=FALSE,
  main="Sound travels from right to left")
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
