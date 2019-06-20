## condition for being available:
## "event1" 1st event (move or vocal)
## "det1" 1st detection (inflates time-to-detection)
## "alldet" count all detections (double counting)
## "present" events are not necessary
bsims_transcribe <-
function(
  x,
  tint=NULL,
  rint=Inf,
  error=0,
  condition=c("event1", "det1", "alldet"),
  event_type=c("vocal", "move", "both"),
  ...) {
  if (!inherits(x, "bsims_detections")) {
    if (inherits(x, "bsims_events")) {
      x <- bsims_detect(x, tau=Inf) # detect all
    } else {
      if (inherits(x, "bsims_population")) {
        x <- bsims_animate(x, initial_location=TRUE) # avail=presence
        x <- bsims_detect(x, tau=Inf) # detect all
      } else {
        stop("x must be a bsims_population/events/detections object")
      }
    }
  }
  condition <- match.arg(condition)
  event_type <- match.arg(event_type)
  ## availability overridden
  if (x$initial_location)
    tint <- x$duration
  tint <- if (is.null(tint))
    x$duration else sort(tint)
  if (any(tint <= 0))
    stop("tint must be > 0")
  if (any(tint > x$duration))
    stop("tint must be <= duration")
  rint <- sort(rint)
  if (any(rint <= 0))
    stop("rint must be > 0")
  ## let get_detections take care of subsetting
  detall <- get_detections(x,
    condition=condition, event_type=event_type)
  ## add distance estimation error
  if (error < 0)
    stop("error must be >= 0")
  derr <- if (error > 0)
    rlnorm2(nrow(detall), detall$d, error) else detall$d
  detall$error <- derr - detall$d
  ## assign labels
  rLAB <- paste0(c(0, round(100*rint[-length(rint)])),
    ifelse(is.finite(rint), paste0("-", round(100*rint)), "+"), "m")
  tLAB <- paste0(c(0, round(tint[-length(tint)], 2)), "-", tint, "min")
  detall$rint <- factor(rLAB[cut(derr, c(0, rint), labels=FALSE,
    include.lowest=TRUE)], rLAB)
  detall$tint <- factor(tLAB[cut(detall$t, c(0, tint), labels=FALSE,
    include.lowest=TRUE)], tLAB)
  ## truncate distances based on percieved distance:
  ## it is when $rint is NA
  ## (but detection probability still depends on actual distance)
  detall <- detall[!is.na(detall$rint),,drop=FALSE]
  ## exclude out of duration (NA) $tint values
  detall <- detall[!is.na(detall$tint),,drop=FALSE]
  ## crosstab
  xt <- as.matrix(Xtab(~ rint + tint, detall))

  x$detections <- detall
  x$removal <- xt
  x$tint <- tint
  x$rint <- rint
  x$error <- error
  x$condition <- condition
  x$event_type <- event_type
  x$call <- match.call()
  class(x) <- c("bsims", "bsims_transcript")
  x
}
