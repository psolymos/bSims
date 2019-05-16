\name{dist_fun2}
\alias{dist_fun2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dist_fun2(d, tau, dist_fun, b = numeric(0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{tau}{
%%     ~~Describe \code{tau} here~~
}
  \item{dist_fun}{
%%     ~~Describe \code{dist_fun} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (d, tau, dist_fun, b = numeric(0)) 
{
    if (any(is.infinite(b))) 
        stop("b must be finite")
    if (length(b) != length(tau) - 1L) 
        stop("length(b) must equal length(tau)-1")
    if (length(b) != length(unique(b))) 
        stop("values in b must be unique")
    b <- sort(b)
    b[b == 0] <- .Machine$double.eps
    h <- 1
    for (i in seq_len(length(b))) {
        h <- c(h, h[length(h)] * dist_fun(b[i], tau[i])/dist_fun(b[i], 
            tau[i + 1L]))
    }
    j <- cut(d, c(0, b, Inf), labels = FALSE, include.lowest = TRUE)
    dist_fun(d, tau[j]) * h[j]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
