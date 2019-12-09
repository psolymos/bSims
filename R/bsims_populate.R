bsims_populate <-
function(
  x, # landscape object
  density=1, # D, recycled 3x for HER
  abund_fun=NULL, # N ~ Pois(lambda), lambda=DA
  xy_fun=NULL, # NULL ~ CSR complete spatial randomness
  margin=0, # margin to pass to xy_fun for edge effect, units as in extent
  maxit=100, # x N times to try
  fail=FALSE,
  ...)
{
  if (!inherits(x, "bsims_landscape"))
    stop("x must be a bsims_landscape object")
  A <- diff(x$strata) * diff(range(x$strata))
  if (!is.null(names(density)))
    density <- density[c("H", "E", "R")]
  if (!(length(density) == 1L || length(density) == 3L))
    stop("density must be of length 1 (H) or 3 (HER)")
  D <- rep(density, 3)[c(1,2,3,2,1)]
  lambda <- A * D
  if (is.null(abund_fun))
    abund_fun <- function(lambda, ...) rpois(1, lambda)
  N <- sapply(lambda, abund_fun, ...)
  if (!all(round(N) == N))
    stop("abund_fun must return integers")
  if (any(N < 0))
    stop("abund_fun must return non-negative integers")
  if (any(is.na(N)))
    stop("abund_fun must not return NA values")
  if (any(is.infinite(N)))
    stop("abund_fun must not return infinite values")
  names(A) <- names(D) <- names(N) <- names(lambda) <-
    c("+H", "+E", "R", "E+", "H+")
  d <- NULL
  for (i in 1:5) {
    z <-data.frame(
      acceptreject(n=N[i], f=xy_fun,
        x0=x$strata[i], x1=x$strata[i+1],
        y0=x$strata[1], y1=x$strata[6],
        m=margin,
        maxit=maxit,
        fail=fail),
      s=rep(i, N[i]))
    # add here spatial non-randomness
    d <- rbind(d, z)
  }
  d$i <- seq_len(nrow(d))
  d$s <- factor(c("H", "E", "R", "E", "H")[d$s], c("H", "E", "R"))
  d <- d[,c("i", "s", "x", "y")]
  x$nests <- d
  ## tessellation
  if (sum(N) > 0) {
    x$tess <- deldir(x$nests$x, x$nests$y, suppressMsge=TRUE)
    x$tess$tile_list <- tile.list(x$tess)
  } else {
    x$tess <- NULL
  }
  x$abund_fun <- abund_fun
  x$sp_settings <- list(
    xy_fun = xy_fun,
    margin = margin,
    maxit = maxit,
    fail = fail)
  x$abundance <- N
  x$lambda <- lambda
  x$area <- A
  x$density <- D
  x$call <- match.call()
  class(x) <- c("bsims_population",
                "bsims_landscape",
                "bsims")
  x
}
