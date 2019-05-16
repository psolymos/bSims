\name{events}
\alias{events}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
events(vocal_rate = 1, move_rate = 1, duration = 10, movement = 0, avoid = c(0, 0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
  \item{avoid}{
%%     ~~Describe \code{avoid} here~~
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
function (vocal_rate = 1, move_rate = 1, duration = 10, movement = 0, 
    avoid = c(0, 0)) 
{
    ev <- cumsum(timetoevent(vocal_rate, duration))
    em <- cumsum(timetoevent(move_rate, duration))
    iv <- rep(1, length(ev))
    im <- rep(0, length(em))
    dv <- matrix(NA, length(ev), 2)
    dm <- rmvn(length(em), c(0, 0), diag(movement^2, 2, 2))
    dm <- dm[dm[, 1] \%][\% avoid, , drop = FALSE]
    while (nrow(dm) < length(em)) {
        dm <- rbind(dm, rmvn(length(em), c(0, 0), diag(movement^2, 
            2, 2)))
        dm <- dm[dm[, 1] \%][\% avoid, , drop = FALSE]
    }
    dm <- dm[seq_along(em), , drop = FALSE]
    h <- cbind(rbind(dv, dm), c(ev, em), c(iv, im))
    colnames(h) <- c("x", "y", "t", "v")
    o <- order(h[, "t"])
    h <- as.data.frame(h[o, , drop = FALSE])
    for (i in which(is.na(h[, "x"]))) {
        if (i == 1L) {
            h$x[i] <- 0
            h$y[i] <- 0
        }
        else {
            h$x[i] <- h$x[i - 1L]
            h$y[i] <- h$y[i - 1L]
        }
    }
    h[h$t <= duration, , drop = FALSE]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
