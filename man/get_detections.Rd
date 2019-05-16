\name{get_detections}
\alias{get_detections}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_detections(x, first_only = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{first_only}{
%%     ~~Describe \code{first_only} here~~
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
function (x, first_only = TRUE) 
{
    if (sum(x$abundance) == 0) 
        return(data.frame(x = numeric(0), y = numeric(0), t = numeric(0), 
            v = numeric(0), d = numeric(0), i = numeric(0)))
    z <- lapply(1:length(x$events), function(i) {
        zz <- x$events[[i]]
        zz$i <- i
        zz <- zz[!is.na(zz$d), , drop = FALSE]
        zz
    })
    z <- do.call(rbind, z)
    z <- z[order(z$t), ]
    if (first_only) 
        z <- z[!duplicated(z$i), , drop = FALSE]
    rownames(z) <- NULL
    z$x <- x$nests$x[z$i] + z$x
    z$y <- x$nests$y[z$i] + z$y
    z$a <- 180 * atan2(z$x, z$y)/pi
    z$a[z$a < 0] <- 360 + z$a[z$a < 0]
    attr(z, "observer") <- x$xy
    z
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
