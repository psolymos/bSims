\name{expand_list}
\alias{expand_list}
\title{
Create a list from all combinations of arguments
}
\description{
Create a list frame from all combinations of the supplied vectors or lists.
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
The nested elements are labelled by the factors.
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
  xy_fun = list(function(z) z, function(z) z * 2))
b[[1]]
str(b)
}
\keyword{ manip }
