\name{rmvn}
\alias{rmvn}
\title{
Multivariate normal distribution
}
\description{
A shim of \code{\link[MASS]{mvrnorm}} to return matrix when n < 2.
}
\usage{
rmvn(n = 1L, mu, Sigma, ...)
}
\arguments{
  \item{n}{
number of random vectors desired (nonnegative integer, can be 0).
}
  \item{mu}{
mean vector.
}
  \item{Sigma}{
variance-covariance matrix.
}
  \item{\dots}{
other arguments passed to  \code{\link[MASS]{mvrnorm}}.
}
}
\value{
A matrix with \code{n} rows and \code{length(mu)} columns.
}
\author{
Peter Solymos
}
\seealso{
\code{\link[MASS]{mvrnorm}}
}
\examples{
rmvn(0, c(a=0, b=0), diag(1, 2, 2))
rmvn(1, c(a=0, b=0), diag(1, 2, 2))
rmvn(2, c(a=0, b=0), diag(1, 2, 2))

sapply(0:10, function(n) dim(rmvn(n, c(a=0, b=0), diag(1, 2, 2))))
}
\keyword{ datagen }
\keyword{ distribution }
