\name{lines.bsims_detections}
\alias{lines.bsims_detections}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lines.bsims_detections(x, first_only = TRUE, tlim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{first_only}{
%%     ~~Describe \code{first_only} here~~
}
  \item{tlim}{
%%     ~~Describe \code{tlim} here~~
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
function (x, first_only = TRUE, tlim = NULL, ...) 
{
    if (is.null(tlim)) 
        tlim <- c(0, x$duration)
    z <- get_detections(x, first_only)
    z <- z[z$t \%[]\% tlim, , drop = FALSE]
    xy <- z[, c("x", "y")]
    segments(x0 = rep(x$xy[1L], nrow(xy)), y0 = rep(x$xy[2L], 
        nrow(xy)), x1 = xy[, 1L], y1 = xy[, 2L], ...)
    invisible(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
