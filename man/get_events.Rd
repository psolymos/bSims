\name{get_events}
\alias{get_events}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_events(x, vocal_only = TRUE, tlim = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{vocal_only}{
%%     ~~Describe \code{vocal_only} here~~
}
  \item{tlim}{
%%     ~~Describe \code{tlim} here~~
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
function (x, vocal_only = TRUE, tlim = NULL) 
{
    if (sum(x$abundance) == 0) 
        return(data.frame(x = numeric(0), y = numeric(0), t = numeric(0), 
            v = numeric(0), i = numeric(0)))
    if (is.null(tlim)) 
        tlim <- c(0, x$duration)
    tlim <- pmin(pmax(0, tlim), x$duration)
    z <- lapply(1:length(x$events), function(i) {
        zz <- x$events[[i]]
        zz$i <- i
        zz
    })
    z <- do.call(rbind, z)
    z <- z[order(z$t), ]
    if (vocal_only) 
        z <- z[z$v > 0, , drop = FALSE]
    rownames(z) <- NULL
    z$x <- x$nests$x[z$i] + z$x
    z$y <- x$nests$y[z$i] + z$y
    z <- z[z$t \%[]\% tlim, , drop = FALSE]
    z
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
