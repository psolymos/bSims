\name{estimate}
\alias{estimate}
\alias{estimate.bsims_transcript}
\title{
Estimate basis parameters
}
\description{
Estimate singing rates, effective distances, and density
based on simulation objects
}
\usage{
estimate(object, ...)
\method{estimate}{bsims_transcript}(object, ...)
}
\arguments{
  \item{object}{
simulation object.
}
  \item{\dots}{
other arguments passed to internal functions.
}
}
\details{
The method evaluates removal design to estimate
model parameters and density using the QPAD methodology
using the 'detect' package.

The function only works with multiple time and
distance intervals. It returns \code{NA} otherwise.
}
\value{
A vector with values for singing rate (phi),
effective detection distance (tau),
and dnesity.
}
\author{
Peter Solymos
}
\seealso{
\code{\link{bsims_init}}
}
\examples{
set.seed(2)
phi <- 0.5                 # singing rate
tau <- 1                   # EDR by strata
dur <- 10                  # simulation duration
tbr <- c(2, 4, 6, 8, 10)   # time intervals
rbr <- c(0.5, 1, 1.5, Inf) # counting radii

l <- bsims_init(10, 0.5, 1)# landscape
p <- bsims_populate(l, 10)  # population
e <- bsims_animate(p,      # events
  vocal_rate=phi, duration=dur)
d <- bsims_detect(e,       # detections
  tau=tau)
x <- bsims_transcribe(d,   # transcription
  tint=tbr, rint=rbr)

estimate(x)
}
\keyword{ models }
