\name{expand_list}
\alias{expand_list}
\title{
Create a list from all combinations of arguments
}
\description{
Create a list from all combinations of the supplied vectors or lists.
}
\usage{
expand_list(...)
}
\arguments{
  \item{\dots}{
vectors or lists. All arguments must be named.
}
}
\value{
A list containing one element for each combination of the supplied vectors
and lists. The first factors vary fastest.
The nested elements are labeled by the factors.

The function allows list elements to be vectors, functions, or \code{NULL}.
If a vector element is supposed to be kept as a vector, use \code{list()}.
}
\author{
Peter Solymos
}
\seealso{
\code{\link{expand.grid}}
}
\examples{
b <- expand_list(
  movement = c(0, 1, 2),
  rint = list(c(0.5, 1, 1.5, Inf)), # in a list to keep as one
  xy_fun = list(NULL, function(z) z))
b[[1]]
str(b)
}
\keyword{ manip }
