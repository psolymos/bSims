\name{plot.bsims_landscape}
\alias{plot.bsims_landscape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.bsims_landscape(x, col = c("darkolivegreen1", "burlywood1", "lightgrey"), xlim = NULL, ylim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
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
function (x, col = c("darkolivegreen1", "burlywood1", "lightgrey"), 
    xlim = NULL, ylim = NULL, ...) 
{
    A <- diff(x$strata) * diff(range(x$strata))
    A <- c(h = A[1] + A[5], e = A[2] + A[4], r = A[3])
    names(A) <- c("H", "E", "R")
    plot(0, type = "n", xlim = if (is.null(xlim)) 
        range(x$box[, "x"])
    else xlim, ylim = if (is.null(ylim)) 
        range(x$box[, "y"])
    else ylim, xlab = "", ylab = "", axes = FALSE, asp = 1, ...)
    if (A[1] > 0) 
        polygon(x$box, col = col[1], border = NA)
    if (A[2] > 0) 
        polygon(x$strata[c("he", "he", "eh", "eh")], x$strata[c("+h", 
            "h+", "h+", "+h")], col = col[2], border = NA)
    if (A[3] > 0) 
        polygon(x$strata[c("er", "er", "re", "re")], x$strata[c("+h", 
            "h+", "h+", "+h")], col = col[3], border = NA)
    invisible(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
