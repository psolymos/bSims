\name{rlnorm2}
\alias{rlnorm2}
\title{
Reparametrized lognormal distribution
}
\description{
A lognormal distribution parametrized as mean (ybar) and SDlog.
}
\usage{
rlnorm2(n, mean = exp(0.5), sdlog = 1)
}
\arguments{
  \item{n}{
number of random numbers desired.
}
  \item{mean}{
mean.
}
  \item{sdlog}{
log scale standard deviation.
}
}
\details{
Log scale mean is \code{log(mean) - sdlog^2/2}.
}
\value{
Vector of random numbers.
}
\author{
Peter Solymos
}
\seealso{
\code{link{rlnorm}}
}
\examples{
summary(rlnorm2(10^6, 1.3, 0.5)) # mean ~ 1.3
exp(log(1.3) - 0.5^2/2) # ~ median
}
\keyword{ datagen }
\keyword{ distribution }
