## tau can be:
## - single number (all the same)
## - length 2 vector: vocal & move
## - length 3 vector: HER attenuation
## - 3 x 2 matrix: HER x vocal/move
## (does not have to match event_type but the 2 are related)
bsims_detect <-
function(
  x,
  xy=c(0,0), # observer location
  tau=1,
  dist_fun=NULL, # takes args d(istance) and tau (single parameter)
  event_type=c("vocal", "move", "both"),
  sensitivity=1,
  direction=FALSE,
  ...)
{
  if (!inherits(x, "bsims_events"))
    stop("x must be a bsims_events object")
  event_type <- match.arg(event_type)
  ## event type is silently ignored
  if (x$initial_location) {
    event_type <- "vocal"  # this is the default arg value
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
  ## sensitivity: of the receiver --> affects the slope of dist fun
  if (length(sensitivity) > 1) {
    sensitivity <- sensitivity[c("vocal", "move")]
  }
  if (any(is.na(sensitivity)))
    stop("something isn't right with sensitivity")
  if (any(sensitivity < 0))
    stop("sensitivity cannot be negative")
  ## direction: logical, only applies to vocalizations
  ## describing anisotropic sound attenuation
  ## away from the sound source
  ## and is a function of direction relative to observer
  direction <- as.logical(direction[1L])
  ## tau is tau, sensitivity is sensitivity
  ## theta ($f) is the angle (0-180)
  ## value is tau when theta=0, sensitivity*tau when theta=180
  dir_fun <- function(tau, sensitivity, theta) {
    h <- (sensitivity*tau)-tau
    tau + h*(0.5-cos(theta*pi/180)/2)
  }
  ## tau processing
  tau <- unname(tau)
  if (length(tau) == 1L) {
    if (length(sensitivity) > 1)
      stop("sensitivity must be of length 1 to match tau")
    tau_type <- "one" # all the same
    tau_use <- tau
  } else {
    if (is.null(dim(tau))) {
      if (length(tau) == 2L) {
        if (length(sensitivity) == 1)
          sensitivity <- c(vocal=sensitivity, move=sensitivity)
        tau_type <- "vm" # vocal/movement
        names(tau) <- c("vocal", "move")
        tau_use <- tau
      } else {
        if (length(tau) == 3L) {
          if (length(sensitivity) > 1)
            stop("sensitivity must be of length 1 to match tau")
          ## this is silent, but makes sense
          if (A["H"] == sum(A)) {
            tau_type <- "one"
            tau_use <- tau[1L]
          } else {
            tau_type <- "her" # H/E/R
            tau_use <- tau
            names(tau_use) <- c("H", "E", "R")
          }
          names(tau) <- c("H", "E", "R")
        } else {
          stop("tau length must must be in 1:3 (or a 2 x 3 matrix)")
        }
      }
    } else {
      if (all(dim(tau) != c(3L, 2L)))
        stop("tau dimension must be 3 x 2")
      if (length(sensitivity) == 1)
        sensitivity <- c(vocal=sensitivity, move=sensitivity)
      if (A["H"] == sum(A)) {
        tau_type <- "vm"
        tau_use <- tau[1L,1:2,drop=TRUE]
        names(tau_use) <- c("vocal", "move")
      } else {
        tau_type <- "six" # her x vm
      }
      dimnames(tau) <- list(c("H", "E", "R"), c("vocal", "move"))
    }
  }
  ## needed for HER *att*enuation
  att <- tau_type %in% c("her", "six") && A["H"] < sum(A)
  if (att) {
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
    ## direction: 0=towards observer, 1=180 degrees away
    ## angle from bird to observer relative to north, clockwise
    if (direction) {
      theta_obs <- round(270-180*atan2(yy, xx)/pi) %% 360
      z$f <- abs(z$a - theta_obs) %% 180
      z$f[z$v==0] <- 0 # movement gets 0 angle --> dist fun unmodified
    } else {
      z$f <- rep(0, nrow(z)) # no anisotropy
    }
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
        if (tau_type == "her") {
          ## order tau as HEREH
          #TAU <- tau[c(1,2,3,2,1)][sobs:sbrd[j]] # obs-->brd ordering = wrong!
          TAU <- tau[c(1,2,3,2,1)][sbrd[j]:sobs] # brd-->obs ordering = good!
          ## calculate distance breaks from x and theta
          if (length(TAU) == 1L) {
            TAUsdir <- if (direction)
              dir_fun(TAU, sensitivity, z$f[j]) else TAU*sensitivity
            q[j] <- dist_fun(z$d[j], TAUsdir, ...)
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
            TAUsdir <- if (direction)
              dir_fun(TAU, sensitivity, z$f[j]) else TAU*sensitivity
            q[j] <- dist_fun2(z$d[j], TAUsdir, dist_fun, b, ...)
          }
        } else { # tau_type == "six"
          ## order tau as HEREH
          #TAUv <- tau[c(1,2,3,2,1),"vocal"][sbrd[j]:sobs]*sensitivity["vocal"]
          TAUv <- if (direction) {
            dir_fun(tau[c(1,2,3,2,1),"vocal"][sbrd[j]:sobs],
                    sensitivity["vocal"],
                    z$f[j])
          } else {
            tau[c(1,2,3,2,1),"vocal"][sbrd[j]:sobs]*sensitivity["vocal"]
          }
          TAUm <- tau[c(1,2,3,2,1),"move"][sbrd[j]:sobs]*sensitivity["move"]
          if (length(TAUv) == 1L) {
            q[j] <- dist_fun(z$d[j],
                if (z$v[j] > 0) TAUv else TAUm, ...)
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
            q[j] <- dist_fun2(z$d[j],
                if (z$v[j] > 0) TAUv else TAUm,
                dist_fun, b, ...)
          }
        }
      }
    } else {
      if (tau_type == "vm") {
        TAUvsdir <- if (direction) {
          dir_fun(tau_use["vocal"], sensitivity["vocal"], z$f)
        } else {
          tau_use["vocal"]*sensitivity["vocal"]
        }
        q <- ifelse(z$v > 0,
          dist_fun(z$d, TAUvsdir, ...),
          dist_fun(z$d, tau_use["move"]*sensitivity["move"], ...))
      } else { # tau_type == "one"
        if (length(sensitivity) > 1)
          stop("sensitivity must be of length 1 to match tau")
        TAUsdir <- if (direction)
          dir_fun(tau_use, sensitivity, z$f) else tau_use
        q <- dist_fun(z$d, TAUsdir, ...)
      }
    }

    z$det <- rbinom(length(z$d), size=1, prob=q)
    z <- z[z$det > 0,,drop=FALSE]
    ## distance is shown where detected, NA when not detected
    x$events[[i]]$d <- z$d[match(rownames(x$events[[i]]), rownames(z))]
    ## $f is the angle between line to obs and face of bird ($a), or NA
    x$events[[i]]$f <- if (direction) {
      z$f[match(rownames(x$events[[i]]), rownames(z))]
    } else {
      rep(NA, nrow(x$events[[i]]))
    }
  }
  x$xy <- xy
  x$tau <- tau
  x$dist_fun <- dist_fun
  x$event_type <- event_type
  x$sensitivity <- sensitivity
  x$direction <- direction
  x$call <- match.call()
  class(x) <- c("bsims_detections",
                "bsims_events",
                "bsims_population",
                "bsims_landscape",
                "bsims")
  x
}
