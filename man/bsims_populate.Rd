\name{bsims_populate}
\alias{bsims_populate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bsims_populate(x, density = 1, abund_fun = NULL, xy_fun = NULL, margin = 0, maxit = 100, fail = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{density}{
%%     ~~Describe \code{density} here~~
}
  \item{abund_fun}{
%%     ~~Describe \code{abund_fun} here~~
}
  \item{xy_fun}{
%%     ~~Describe \code{xy_fun} here~~
}
  \item{margin}{
%%     ~~Describe \code{margin} here~~
}
  \item{maxit}{
%%     ~~Describe \code{maxit} here~~
}
  \item{fail}{
%%     ~~Describe \code{fail} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, density = 1, abund_fun = NULL, xy_fun = NULL, margin = 0, 
    maxit = 100, fail = FALSE, ...) 
{
    if (!inherits(x, "bsims_landscape")) 
        stop("x must be a bsims_landscape object")
    A <- diff(x$strata) * diff(range(x$strata))
    D <- rep(density, 3)[c(1, 2, 3, 2, 1)]
    lambda <- A * D
    if (is.null(abund_fun)) 
        abund_fun <- function(lambda, ...) rpois(1, lambda)
    N <- sapply(lambda, abund_fun, ...)
    names(A) <- names(D) <- names(N) <- names(lambda) <- c("+H", 
        "+E", "R", "E+", "H+")
    d <- NULL
    for (i in 1:5) {
        z <- data.frame(acceptreject(n = N[i], f = xy_fun, x0 = x$strata[i], 
            x1 = x$strata[i + 1], y0 = x$strata[1], y1 = x$strata[6], 
            m = margin, maxit = maxit, fail = fail), s = rep(i, 
            N[i]))
        d <- rbind(d, z)
    }
    d$i <- seq_len(nrow(d))
    d$s <- factor(c("H", "E", "R", "E", "H")[d$s], c("H", "E", 
        "R"))
    d <- d[, c("i", "s", "x", "y")]
    x$nests <- d
    x$abundance <- N
    x$lambda <- lambda
    x$area <- A
    x$density <- D
    class(x) <- c("bsim", "bsims_population")
    x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
