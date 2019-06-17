bsims_animate <-
function(
  x, # population object
  vocal_rate=1, # phi /min
  move_rate=1, #movement
  duration=10,
  movement=0, # SD for 2D kernel
  mixture=1, # finite mixture group proportions
  avoid=c("none", "R", "ER"),
  initial_location=FALSE,
  ...) {
  if (!inherits(x, "bsims_population"))
    stop("x must be a bsims_population object")
  avoid <- match.arg(avoid)
  if (avoid == "ER" && sum(x$density[2:4]) > 0)
    stop(">0 density ER strata cannot be avoided")
  if (avoid == "R" && sum(x$density[3]) > 0)
    stop(">0 density R stratum cannot be avoided")
  if (movement < 0)
    stop("movement can not be negative")
  if (any(mixture < 0))
    stop("mixture must not be negative")
  if (move_rate < 0)
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
      if (dim(vocal_rate) != c(3L, K))
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
  dimnames(vr) <- dimnames(mr) <- list(c("H", "E", "R"), G)
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
      if (move_rate > 0)
        warning("initial_location=TRUE: move_rate ignored")
      if (vocal_rate > 0)
        warning("initial_location=TRUE: vocal_rate ignored")
      Events[[i]] <- data.frame(x=0, y=0, t=0, v=0)
    } else {
      Events[[i]] <- events(
        vocal_rate=vr[s[i], g[i]],
        move_rate=mr[s[i], g[i]],
        duration=duration,
        movement=movement,
        avoid=a)
    }
  }
  x$vocal_rate <- vr
  x$move_rate <- mr
  x$duration <- duration
  x$movement <- movement
  x$mixture <- P
  x$events <- Events
  x$call <- match.call()
  class(x) <- c("bsims", "bsims_events")
  x
}
