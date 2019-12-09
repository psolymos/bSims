bsims_detect <-
function(
  x,
  xy=c(0,0), # observer location
  tau=1, # can be vector when HER attenuation used, compatible w/ dist_fun
  dist_fun=NULL, # takes args d and tau (single parameter)
  event_type=c("vocal", "move", "both"),
  ...)
{
  if (!inherits(x, "bsims_events"))
    stop("x must be a bsims_events object")
  event_type <- match.arg(event_type)
  if (x$initial_location) {
    event_type <- "both"
  }
  xy <- as.numeric(xy[1:2])
  if (any(xy %)(% range(x$strata)))
    stop("observer xy must be within extent")
  if (is.null(dist_fun))
    dist_fun <- function(d, tau, ...) exp(-d^2/tau^2)
  N <- sum(x$abundance)
  A <- diff(x$strata) * diff(range(x$strata))
  A <- c(h=A[1]+A[5], e=A[2]+A[4], r=A[3])
  names(A) <- c("H", "E", "R")
  ## this is silent, but makes sense
  if (length(tau) > 1L && A["H"] == sum(A))
    tau <- tau[1L]
  ## needed for *att*enuation
  att <- length(tau) > 1L && A["H"] < sum(A)
  if (att && length(tau) != 3L)
    stop("tau length must be 3 for HER attenuation")
  if (att) {
    names(tau) <- c("H", "E", "R")
    st <- x$strata
    st[1L] <- -Inf
    st[6L] <- Inf
    eps <- 0.00001 # this is 1 mm
    for (i in which(duplicated(st))) {
      st[i] <- st[i-1] + ifelse(st[i] < 0, -eps, eps)
    }
    sobs <- cut(xy[1L], st, labels=FALSE) # which stratum the observer is in
  }
  for (i in seq_len(N)) {
    z <- x$events[[i]]
    ## bird position
    xxb <- x$nests$x[i] + z$x
    yyb <- x$nests$y[i] + z$y
    ## distance from observer
    xx <- xxb - xy[1L]
    yy <- yyb - xy[2L]
    z$d <- sqrt(xx^2 + yy^2) # distance from observer
    ## repel inds within repel distance (0 just for temp object z)
#    z$v[z$d < repel] <- 0
    ## NA is placeholder for these vocalizations in return object
#    x$events[[i]]$v[z$d < repel] <- NA
    ## subset based on event type requested
    keep <- switch(event_type,
      "vocal"=z$v > 0,
      "move"=z$v == 0,
      "both"=rep(TRUE, nrow(z)))
    if (x$initial_location)
      keep <- rep(TRUE, nrow(z))
    z <- z[keep,,drop=FALSE]
    xx <- xx[keep]
    yy <- yy[keep]
    xxb <- xxb[keep]
    yyb <- yyb[keep]
    ## this is where HER attenuation comes in
    if (att) {
      q <- numeric(nrow(z))
      theta <- atan2(yy, xx) # angle in rad
      sbrd <- cut(xxb, st, labels=FALSE)
      for (j in seq_len(nrow(z))) {
        ## order tau as HEREH
        #TAU <- tau[c(1,2,3,2,1)][sobs:sbrd[j]] # obs-->brd ordering = wrong!
        TAU <- tau[c(1,2,3,2,1)][sbrd[j]:sobs] # brd-->obs ordering = good!
        ## calculate distance breaks from x and theta
        if (length(TAU) == 1L) {
          #b <- numeric(0)
          q[j] <- dist_fun(z$d[j], TAU, ...)
        } else {
          ## this gives breaks along x axis
          if (sobs < sbrd[j]) { # bird right of observer
            stj <- st[(sobs+1):sbrd[j]]
          } else { # bird left of observer
            stj <- st[sobs:(sbrd[j]+1)]
          }
          ## breaks as radial distance from observer: r=x/cos(theta)
          b <- (stj - xy[1L]) / cos(theta[j])
          ## turn that into distance from bird (stratified attenuation)
          b <- z$d[j] - rev(b)
          ## calculate q
          q[j] <- dist_fun2(z$d[j], TAU, dist_fun, b, ...)
        }
      }
    } else {
      q <- dist_fun(z$d, tau, ...)
    }
    z$det <- rbinom(length(z$d), size=1, prob=q)
    z <- z[z$det > 0,,drop=FALSE]
    ## distance is shown where detected, NA when not detected
    x$events[[i]]$d <- z$d[match(rownames(x$events[[i]]), rownames(z))]
  }
  x$xy <- xy
  x$tau <- tau
  x$dist_fun <- dist_fun
  x$event_type <- event_type
  x$call <- match.call()
  class(x) <- c("bsims_detections",
                "bsims_events",
                "bsims_population",
                "bsims_landscape",
                "bsims")
  x
}
