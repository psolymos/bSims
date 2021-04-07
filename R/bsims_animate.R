bsims_animate <-
function(
  x, # population object
  vocal_rate=1, # phi /min
  move_rate=0, #movement
  duration=10,
  movement=0, # SD for 2D kernel
  mixture=1, # finite mixture group proportions
  avoid=c("none", "R", "ER"),
  initial_location=FALSE,
  allow_overlap=TRUE,
  ...) {
  if (!inherits(x, "bsims_population"))
    stop("x must be a bsims_population object")
  avoid <- match.arg(avoid)
  if (avoid == "ER" && sum(x$density[2:4]) > 0)
    stop(">0 density ER strata cannot be avoided")
  if (avoid == "R" && sum(x$density[3]) > 0)
    stop(">0 density R stratum cannot be avoided")
  if (any(movement < 0))
    stop("movement can not be negative")
  if (any(mixture < 0))
    stop("mixture must not be negative")
  if (any(move_rate < 0))
    stop("move_rate must not be negative")
  if (any(vocal_rate < 0))
    stop("vocal_rate must not be negative")
  K <- length(mixture)
  G <- paste0("G", 1:K)
  P <- structure(mixture / sum(mixture), names=G)
  ## vocal rate processing
  if (length(vocal_rate) == 1L) {
    vr <- matrix(vocal_rate, 3, K)
  } else {
    if (is.null(dim(vocal_rate))) {
      if (K > 1L) {
        if (length(vocal_rate) != K)
          stop("vocal_rate length must equal mixture length")
        vr <- matrix(vocal_rate, 3, K, byrow=TRUE)
      } else {
        if (length(vocal_rate) != 3L)
          stop("vocal_rate length must equal 3 when length(mixture)=1")
        vr <- matrix(vocal_rate, 3, K)
      }
    } else {
      if (all(dim(vocal_rate) != c(3L, K)))
        stop("vocal_rate dimension must be 3 x length(mixture)")
      vr <- vocal_rate
    }
  }
  ## movement rate processing
  if (length(move_rate) == 1L) {
    mr <- matrix(move_rate, 3, K)
  } else {
    if (is.null(dim(move_rate))) {
      if (K > 1L) {
        if (length(move_rate) != K)
          stop("move_rate length must equal mixture length")
        mr <- matrix(move_rate, 3, K, byrow=TRUE)
      } else {
        if (length(move_rate) != 3L)
          stop("move_rate length must equal 3 when length(mixture)=1")
        mr <- matrix(move_rate, 3, K)
      }
    } else {
      if (dim(move_rate) != c(3L, K))
        stop("move_rate dimension must be 3 x length(mixture)")
      mr <- move_rate
    }
  }
  ## movement SD processing
  if (length(movement) == 1L) {
    mSD <- matrix(movement, 3, K)
  } else {
    if (is.null(dim(movement))) {
      if (K > 1L) {
        if (length(movement) != K)
          stop("movement length must equal mixture length")
        mSD <- matrix(movement, 3, K, byrow=TRUE)
      } else {
        if (length(movement) != 3L)
          stop("movement length must equal 3 when length(mixture)=1")
        mSD <- matrix(movement, 3, K)
      }
    } else {
      if (dim(movement) != c(3L, K))
        stop("movement dimension must be 3 x length(mixture)")
      mSD <- movement
    }
  }
  dimnames(vr) <- dimnames(mr) <- dimnames(mSD) <- list(c("H", "E", "R"), G)
  N <- sum(x$abundance)
  g <- sample(G, N, replace=TRUE, prob=P)
  x$nests$g <- factor(g, G)
  s <- as.character(x$nests$s)
  Events <- list()
  for (i in seq_len(N)) {
    a <- switch(avoid,
      "none" = c(0,0),
      "R" = x$strata[c("er", "re")]-x$nests$x[i],
      "ER" = x$strata[c("he", "eh")]-x$nests$x[i])
    if (initial_location) {
      ## resent all values to defaults
      move_rate <- 0
      vocal_rate <- 0
      movement <- 0
      mixture <- 1
      avoid <- "none"
      ## event_type="vocal" is the default downstream thus v=1
      Events[[i]] <- data.frame(x=0, y=0, t=0, v=1)
    } else {
      e <- events(
        vocal_rate=vr[s[i], g[i]],
        move_rate=mr[s[i], g[i]],
        duration=duration,
        movement=mSD[s[i], g[i]],
        avoid=a)
      ## add here tessellation based rules
      if (!allow_overlap && !is.null(x$tess)) {
        ## identify tile id for locations
        ti <- sapply(seq_len(nrow(e)), function(j)
          which.tile(e$x[j]+x$nest$x[i], e$y[j]+x$nest$y[i], x$tess$tile_list))
        ## outside locations are reevaluated
        for (ii in which(ti != i)) {
          tmp <- ti[ii]
          while (tmp != i) {
            dm <- rmvn(1, c(0, 0), diag(mSD[s[i], g[i]]^2, 2, 2))
            tmp <- which.tile(dm[1L]+x$nest$x[i], dm[2L]+x$nest$y[i], x$tess$tile_list)
          }
          e$x[ii] <- dm[1L]
          e$y[ii] <- dm[2L]
        }
      }
      Events[[i]] <- e
    }
    ## direction added for vocalization events (NA for movement)
    ## a=angle in degrees (0:360) relative to north clockwise
    Events[[i]]$a <- ifelse(Events[[i]]$v == 1,
      sample(0:359, nrow(Events[[i]]), replace=TRUE), NA)
  }
  x$vocal_rate <- vr
  x$move_rate <- mr
  x$duration <- duration
  x$movement <- mSD
  x$mixture <- P
  x$avoid <- avoid
  x$initial_location <- initial_location
  x$allow_overlap <- allow_overlap
  x$events <- Events
  x$call <- match.call()
  class(x) <- c("bsims_events",
                "bsims_population",
                "bsims_landscape",
                "bsims")
  x
}
