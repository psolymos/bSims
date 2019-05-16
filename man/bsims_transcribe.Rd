\name{bsims_transcribe}
\alias{bsims_transcribe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bsims_transcribe(x, tint = NULL, rint = Inf, first_only = TRUE, error = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{tint}{
%%     ~~Describe \code{tint} here~~
}
  \item{rint}{
%%     ~~Describe \code{rint} here~~
}
  \item{first_only}{
%%     ~~Describe \code{first_only} here~~
}
  \item{error}{
%%     ~~Describe \code{error} here~~
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
function (x, tint = NULL, rint = Inf, first_only = TRUE, error = 0, 
    ...) 
{
    if (!inherits(x, "bsims_detections")) 
        stop("x must be a bsims_detections object")
    tint <- if (is.null(tint)) 
        x$duration
    else sort(tint)
    if (any(tint <= 0)) 
        stop("tint must be > 0")
    if (any(tint > x$duration)) 
        stop("tint must <= duration")
    rint <- sort(rint)
    if (any(rint <= 0)) 
        stop("rint must be > 0")
    detall <- get_detections(x, first_only = FALSE)
    detall <- detall[detall$d <= max(rint), , drop = FALSE]
    if (error < 0) 
        stop("error must be >= 0")
    derr <- if (error > 0) 
        rlnorm2(nrow(detall), detall$d, error)
    else detall$d
    detall$error <- derr - detall$d
    rLAB <- paste0(c(0, round(100 * rint[-length(rint)])), ifelse(is.finite(rint), 
        paste0("-", round(100 * rint)), "+"), "m")
    tLAB <- paste0(c(0, round(tint[-length(tint)], 2)), "-", 
        tint, "min")
    detall$rint <- factor(rLAB[cut(derr, c(0, rint), labels = FALSE, 
        include.lowest = TRUE)], rLAB)
    detall$tint <- factor(tLAB[cut(detall$t, c(0, tint), labels = FALSE, 
        include.lowest = TRUE)], tLAB)
    det <- detall
    if (first_only) 
        det <- det[!duplicated(det$i), , drop = FALSE]
    xt <- as.matrix(Xtab(~rint + tint, det))
    vis <- xt
    vis[] <- 0
    for (i in tLAB) {
        det2 <- detall[detall$tint == i, , drop = FALSE]
        if (first_only) 
            det2 <- det2[!duplicated(det2$i), , drop = FALSE]
        vis <- vis + as.matrix(Xtab(~rint + tint, det2))
    }
    x$detections <- detall
    x$removal <- xt
    x$visits <- vis
    x$tint <- tint
    x$rint <- rint
    x$first_only <- first_only
    x$error <- error
    class(x) <- c("bsim", "bsims_transcript", "bsims_detections")
    x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
