acceptreject <-
function(
  n, # number of pts
  f=NULL, # function returning probability given distance
  x0=0, x1=1, # x range
  y0=0, y1=1, # y range
  m=0, # margin width for edge effect
  maxit=100, # max iterations to break
  fail=FALSE
) {
  if (n <= 0)
    return(cbind(
        x=numeric(0),
        y=numeric(0)))
  if (is.null(f))
      return(cbind(
        x=runif(n, x0, x1),
        y=runif(n, y0, y1)))
  g <- function(m)
    c(runif(1, x0-m, x1+m), runif(1, y0-1, y1+m))
  xy <- rbind(g(m=0))
  colnames(xy) <- c("x", "y")
  i <- 1L # iterations
  j <- 1L # pts inside bbox
  while (j < n) {
    if (i > maxit*n)
      break
    pr <- g(m=m)
    d <- min(sqrt((xy[,1]-pr[1])^2 + (xy[,2]-pr[2])^2))
    p <- f(d)
    r <- runif(1)
    if (r <= p)
      xy <- rbind(xy, unname(pr))
    i <- i + 1L
    j <- sum(xy[,1] %[]% c(x0, x1) & xy[,2] %[]% c(y0, y1))
  }
  xy <- xy[xy[,1] %[]% c(x0, x1) & xy[,2] %[]% c(y0, y1),,drop=FALSE]
  if (nrow(xy) < n) {
    msg <- sprintf("number of points generated: %.0f", nrow(xy))
    if (fail)
      stop(msg)
    warning(msg)
    xy <- rbind(xy, matrix(NA, n-nrow(xy), 2))
  }
  xy
}
