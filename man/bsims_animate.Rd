\name{bsims_animate}
\alias{bsims_animate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bsims_animate(x, vocal_rate = 1, move_rate = 1, duration = 10, movement = 0, mixture = 1, avoid = c("none", "R", "ER"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{vocal_rate}{
%%     ~~Describe \code{vocal_rate} here~~
}
  \item{move_rate}{
%%     ~~Describe \code{move_rate} here~~
}
  \item{duration}{
%%     ~~Describe \code{duration} here~~
}
  \item{movement}{
%%     ~~Describe \code{movement} here~~
}
  \item{mixture}{
%%     ~~Describe \code{mixture} here~~
}
  \item{avoid}{
%%     ~~Describe \code{avoid} here~~
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
function (x, vocal_rate = 1, move_rate = 1, duration = 10, movement = 0, 
    mixture = 1, avoid = c("none", "R", "ER"), ...) 
{
    if (!inherits(x, "bsims_population")) 
        stop("x must be a bsims_population object")
    avoid <- match.arg(avoid)
    if (avoid == "ER" && sum(x$density[2:4]) > 0) 
        stop(">0 density ER strata cannot be avoided")
    if (avoid == "R" && sum(x$density[3]) > 0) 
        stop(">0 density R stratum cannot be avoided")
    if (movement < 0) 
        stop("movement can not be negative")
    if (any(mixture < 0)) 
        stop("mixture must not be negative")
    if (move_rate < 0) 
        stop("move_rate must not be negative")
    if (vocal_rate < 0) 
        stop("vocal_rate must not be negative")
    K <- length(mixture)
    G <- paste0("G", 1:K)
    P <- structure(mixture/sum(mixture), names = G)
    if (length(vocal_rate) == 1L) {
        vr <- matrix(vocal_rate, 3, K)
    }
    else {
        if (is.null(dim(vocal_rate))) {
            if (K > 1L) {
                if (length(vocal_rate) != K) 
                  stop("vocal_rate length must equal mixture length")
                vr <- matrix(vocal_rate, 3, K, byrow = TRUE)
            }
            else {
                if (length(vocal_rate) != 3L) 
                  stop("vocal_rate length must equal 3 when length(mixture)=1")
                vr <- matrix(vocal_rate, 3, K)
            }
        }
        else {
            if (dim(vocal_rate) != c(3L, K)) 
                stop("vocal_rate dimension must be 3 x length(mixture)")
            vr <- vocal_rate
        }
    }
    if (length(move_rate) == 1L) {
        mr <- matrix(move_rate, 3, K)
    }
    else {
        if (is.null(dim(move_rate))) {
            if (K > 1L) {
                if (length(move_rate) != K) 
                  stop("move_rate length must equal mixture length")
                mr <- matrix(move_rate, 3, K, byrow = TRUE)
            }
            else {
                if (length(move_rate) != 3L) 
                  stop("move_rate length must equal 3 when length(mixture)=1")
                mr <- matrix(move_rate, 3, K)
            }
        }
        else {
            if (dim(move_rate) != c(3L, K)) 
                stop("move_rate dimension must be 3 x length(mixture)")
            mr <- move_rate
        }
    }
    dimnames(vr) <- dimnames(mr) <- list(c("H", "E", "R"), G)
    N <- sum(x$abundance)
    g <- sample(G, N, replace = TRUE, prob = P)
    x$nests$g <- factor(g, G)
    s <- as.character(x$nests$s)
    Events <- list()
    for (i in seq_len(N)) {
        a <- switch(avoid, none = c(0, 0), R = x$strata[c("er", 
            "re")] - x$nests$x[i], ER = x$strata[c("he", "eh")] - 
            x$nests$x[i])
        Events[[i]] <- events(vocal_rate = vr[s[i], g[i]], move_rate = mr[s[i], 
            g[i]], duration = duration, movement = movement, 
            avoid = a)
    }
    x$vocal_rate <- vr
    x$move_rate <- mr
    x$duration <- duration
    x$movement <- movement
    x$mixture <- P
    x$events <- Events
    class(x) <- c("bsim", "bsims_events")
    x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
