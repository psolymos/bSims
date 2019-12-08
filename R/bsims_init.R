bsims_init <-
function(
  extent=10,
  road=0,
  edge=0,
  offset=0
) {
  if (extent <= 0)
    stop("extent must be positive")
  road <- abs(road)
  edge <- abs(edge)
  #if (road == 0 && edge > 0)
  #  stop("edge cannot be > 0 when road = 0")
  a <- extent / 2
  box <- cbind(
    x=rep(a, 4) * c(-1, -1, 1, 1),
    y=rep(a, 4) * c(-1, 1, 1, -1))
  strata <- c(
    -a,
    offset-road-edge,
    offset-road,
    offset+road,
    offset+road+edge,
    a)
  strata <- pmin(a, pmax(-a, strata))
  names(strata) <- c("+h", "he", "er", "re", "eh", "h+")
  x <- list(
    extent=extent,
    road=road,
    edge=edge,
    offset=offset,
    box=box,
    strata=strata)
  x$call <- match.call()
  class(x) <- c("bsims_landscape",
                "bsims")
  x
}
