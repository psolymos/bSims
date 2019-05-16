\name{bsims_detect}
\alias{bsims_detect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bsims_detect(x, xy = c(0, 0), tau = 1, dist_fun = NULL, repel = 0, vocal_only = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{xy}{
%%     ~~Describe \code{xy} here~~
}
  \item{tau}{
%%     ~~Describe \code{tau} here~~
}
  \item{dist_fun}{
%%     ~~Describe \code{dist_fun} here~~
}
  \item{repel}{
%%     ~~Describe \code{repel} here~~
}
  \item{vocal_only}{
%%     ~~Describe \code{vocal_only} here~~
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
function (x, xy = c(0, 0), tau = 1, dist_fun = NULL, repel = 0, 
    vocal_only = TRUE, ...) 
{
    if (!inherits(x, "bsims_events")) 
        stop("x must be a bsims_events object")
    xy <- as.numeric(xy[1:2])
    if (any(xy \%)(\% range(x$strata))) 
        stop("observer xy must be within extent")
    if (is.null(dist_fun)) 
        dist_fun <- function(d, tau) exp(-d^2/tau^2)
    N <- sum(x$abundance)
    A <- diff(x$strata) * diff(range(x$strata))
    A <- c(h = A[1] + A[5], e = A[2] + A[4], r = A[3])
    names(A) <- c("H", "E", "R")
    att <- length(tau) > 1 && A["H"] < sum(A)
    if (att && length(tau) != 3L) 
        stop("tau length must be 3 for HER attenuation")
    if (att) {
        st <- x$strata
        st[1L] <- -Inf
        st[6L] <- Inf
        sobs <- cut(xy[1L], st, labels = FALSE)
    }
    for (i in seq_len(N)) {
        z <- x$events[[i]]
        xxb <- x$nests$x[i] + z$x
        yyb <- x$nests$y[i] + z$y
        xx <- xxb - xy[1L]
        yy <- yyb - xy[2L]
        z$d <- sqrt(xx^2 + yy^2)
        z$v[z$d < repel] <- 0
        x$events[[i]]$v[z$d < repel] <- NA
        if (vocal_only) {
            keep <- z$v > 0
            z <- z[keep, , drop = FALSE]
            xx <- xx[keep]
            yy <- yy[keep]
        }
        if (att) {
            theta <- atan2(yy, xx)
            sbrd <- cut(xxb, st, labels = FALSE)
            for (j in seq_len(nrow(z))) {
                TAU <- tau[c(1, 2, 3, 2, 1)][sobs:sbrd[j]]
                if (length(TAU) == 1) {
                  q <- dist_fun(z$d[j], TAU)
                }
                else {
                  if (sobs < sbrd[j]) {
                    stj <- st[(sobs + 1):sbrd[j]]
                  }
                  else {
                    stj <- st[sobs:(sbrd[j] + 1)]
                  }
                  b <- (stj - xy[1L])/cos(theta[j])
                  q <- dist_fun2(z$d[j], TAU, dist_fun, b)
                }
            }
        }
        else {
            q <- dist_fun(z$d, tau)
        }
        z$det <- rbinom(length(z$d), size = 1, prob = q)
        z <- z[z$det > 0, , drop = FALSE]
        x$events[[i]]$d <- z$d[match(rownames(x$events[[i]]), 
            rownames(z))]
    }
    x$xy <- xy
    x$tau <- tau
    x$repel <- repel
    x$vocal_only <- vocal_only
    class(x) <- c("bsim", "bsims_detections")
    x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
