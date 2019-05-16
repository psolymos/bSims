\name{plot.bsims_events}
\alias{plot.bsims_events}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.bsims_events(x, tlim = NULL, pch_nest = 3, col_nest = "darkgreen", cex_nest = 1, pch_vocal = 21, col_vocal = "blue", cex_vocal = 0.5, lty_move = 1, col_move = "orange", lwd_move = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{tlim}{
%%     ~~Describe \code{tlim} here~~
}
  \item{pch_nest}{
%%     ~~Describe \code{pch_nest} here~~
}
  \item{col_nest}{
%%     ~~Describe \code{col_nest} here~~
}
  \item{cex_nest}{
%%     ~~Describe \code{cex_nest} here~~
}
  \item{pch_vocal}{
%%     ~~Describe \code{pch_vocal} here~~
}
  \item{col_vocal}{
%%     ~~Describe \code{col_vocal} here~~
}
  \item{cex_vocal}{
%%     ~~Describe \code{cex_vocal} here~~
}
  \item{lty_move}{
%%     ~~Describe \code{lty_move} here~~
}
  \item{col_move}{
%%     ~~Describe \code{col_move} here~~
}
  \item{lwd_move}{
%%     ~~Describe \code{lwd_move} here~~
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
function (x, tlim = NULL, pch_nest = 3, col_nest = "darkgreen", 
    cex_nest = 1, pch_vocal = 21, col_vocal = "blue", cex_vocal = 0.5, 
    lty_move = 1, col_move = "orange", lwd_move = 1, ...) 
{
    op <- par(xpd = TRUE)
    on.exit(par(op))
    xx <- x
    class(xx) <- c("bsim", "bsims_population")
    plot(xx, pch_nest = pch_nest, col_nest = col_nest, cex_nest = cex_nest, 
        ...)
    if (!is.na(lty_move)) 
        lines(x, tlim = tlim, col = col_move, lty = lty_move, 
            lwd = lwd_move, ...)
    if (!is.na(pch_vocal)) 
        points(x, vocal_only = TRUE, tlim = tlim, col = col_vocal, 
            pch = pch_vocal, cex = cex_vocal, ...)
    invisible(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
