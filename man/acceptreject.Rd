\name{acceptreject}
\alias{acceptreject}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
acceptreject(n, f = NULL, x0 = 0, x1 = 1, y0 = 0, y1 = 1, m = 0, maxit = 100, fail = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{x0}{
%%     ~~Describe \code{x0} here~~
}
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{y0}{
%%     ~~Describe \code{y0} here~~
}
  \item{y1}{
%%     ~~Describe \code{y1} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{maxit}{
%%     ~~Describe \code{maxit} here~~
}
  \item{fail}{
%%     ~~Describe \code{fail} here~~
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
function (n, f = NULL, x0 = 0, x1 = 1, y0 = 0, y1 = 1, m = 0, 
    maxit = 100, fail = FALSE) 
{
    if (n <= 0) 
        return(cbind(x = numeric(0), y = numeric(0)))
    if (is.null(f)) 
        return(cbind(x = runif(n, x0, x1), y = runif(n, y0, y1)))
    g <- function(m) c(runif(1, x0 - m, x1 + m), runif(1, y0 - 
        1, y1 + m))
    xy <- rbind(g(m = 0))
    colnames(xy) <- c("x", "y")
    i <- 1L
    j <- 1L
    while (j < n) {
        if (i > maxit * n) 
            break
        pr <- g(m = m)
        d <- min(sqrt((xy[, 1] - pr[1])^2 + (xy[, 2] - pr[2])^2))
        p <- f(d)
        r <- runif(1)
        if (r <= p) 
            xy <- rbind(xy, unname(pr))
        i <- i + 1L
        j <- sum(xy[, 1] \%[]\% c(x0, x1) & xy[, 2] \%[]\% c(y0, 
            y1))
    }
    xy <- xy[xy[, 1] \%[]\% c(x0, x1) & xy[, 2] \%[]\% c(y0, y1), 
        , drop = FALSE]
    if (nrow(xy) < n) {
        msg <- sprintf("number of points generated: \%.0f", nrow(xy))
        if (fail) 
            stop(msg)
        warning(msg)
        xy <- rbind(xy, matrix(NA, n - nrow(xy), 2))
    }
    xy
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
