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
  event_type=NULL,
  perception=NULL,
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
  if (is.null(event_type))
    event_type <- x$event_type
  event_type <- match.arg(event_type, c("vocal", "move", "both"))
  ## availability overridden
  if (x$initial_location) {
    tint <- x$duration
    event_type <- "both"
  }
  tint <- if (is.null(tint))
    x$duration else sort(tint)
  if (any(tint <= 0))
    stop("tint must be > 0")
  if (any(tint > x$duration))
    stop("tint must be <= duration")
  rint <- sort(rint)
  if (any(rint <= 0))
    stop("rint must be > 0")
  rLAB <- paste0(c(0, round(100*rint[-length(rint)])),
    ifelse(is.finite(rint), paste0("-", round(100*rint)), "+"), "m")
  tLAB <- paste0(c(0, round(tint[-length(tint)], 2)), "-", tint, "min")

  ## --- REMOVAL ---
  ## let get_detections take care of subsetting
  detrem <- .get_detections(x,
    condition=condition,
    event_type=event_type,
    perception=perception)
  ## add distance estimation error
  if (error < 0)
    stop("error must be >= 0")
  derr <- if (error > 0)
    rlnorm2(nrow(detrem), detrem$d, error) else detrem$d
  detrem$error <- derr - detrem$d
  ## assign labels
  detrem$rint <- factor(rLAB[cut(derr, c(0, rint), labels=FALSE,
    include.lowest=TRUE)], rLAB)
  detrem$tint <- factor(tLAB[cut(detrem$t, c(0, tint), labels=FALSE,
    include.lowest=TRUE)], tLAB)
  ## truncate distances based on percieved distance:
  ## it is when $rint is NA
  ## (but detection probability still depends on actual distance)
  detrem <- detrem[!is.na(detrem$rint),,drop=FALSE]
  ## exclude out of duration (NA) $tint values
  detrem <- detrem[!is.na(detrem$tint),,drop=FALSE]
  ## crosstab
  xtrem <- as.matrix(Xtab(~ rint + tint, detrem))

  ## --- MULTIPLE VISIT ---
  ## let get_detections take care of subsetting
  detvis <- NULL
  for (i in seq_along(tint)) {
    detvis <- rbind(detvis, .get_detections(x,
      condition=condition,
      event_type=event_type,
      perception=perception,
      tlim=c(0, tint)[c(i, i+1L)]))
  }
  ## add distance estimation error
  if (error < 0)
    stop("error must be >= 0")
  derr2 <- if (error > 0)
    rlnorm2(nrow(detvis), detvis$d, error) else detvis$d
  detvis$error <- derr2 - detvis$d
  ## assign labels
  detvis$rint <- factor(rLAB[cut(derr2, c(0, rint), labels=FALSE,
    include.lowest=TRUE)], rLAB)
  detvis$tint <- factor(tLAB[cut(detvis$t, c(0, tint), labels=FALSE,
    include.lowest=TRUE)], tLAB)
  ## truncate distances based on percieved distance:
  ## it is when $rint is NA
  ## (but detection probability still depends on actual distance)
  detvis <- detvis[!is.na(detvis$rint),,drop=FALSE]
  ## exclude out of duration (NA) $tint values
  detvis <- detvis[!is.na(detvis$tint),,drop=FALSE]
  ## crosstab
  xtvis <- as.matrix(Xtab(~ rint + tint, detvis))

  x$detections <- list(removal=detrem, visits=detvis)
  x$removal <- xtrem
  x$visits <- xtvis
  x$tint <- tint
  x$rint <- rint
  x$error <- error
  x$condition <- condition
  x$event_type <- event_type
  x$perception <- perception
  x$call <- match.call()
  class(x) <- c("bsims_transcript",
                "bsims_detections",
                "bsims_events",
                "bsims_population",
                "bsims_landscape",
                "bsims")
  x
}
