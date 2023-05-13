## condition for being available:
## "event1" 1st event (move or vocal)
## "det1" 1st detection (inflates time-to-detection)
## "alldet" count all detections (double counting)
.get_detections <-
function(x,
  condition=c("event1", "det1", "alldet"),
  event_type=c("vocal", "move", "both"),
  tlim=NULL,
  perception=NULL)
{
  condition <- match.arg(condition)
  event_type <- match.arg(event_type)

  ## availability overridden
  if (x$initial_location) {
    tint <- x$duration
    ## this must be overridden in calling functions
    #event_type <- "both"
    tlim <- NULL
  }

  ## get the events
  z <- .get_events(x, event_type=event_type, tlim=tlim)
  if (nrow(z) == 0) {
    z$d <- numeric(0)
    z$j <- z$i # perceived individual id
    return(z)
  }
  ## deal with under/over counting using hclust
  ## note: this is not necessarily meaningful
  ## when condition="alldet" (it is already double counting
  ## to the max degree possible)
  z$j <- z$i
  if (!is.null(perception)) {
    if (!is.numeric(perception))
      stop("perception must be numeric")
    if (perception < 0)
      stop("perception must be >= 0")
    if (nrow(z) >= 2L) {
      hc <- hclust(dist(cbind(z$x, z$y)), method="ward.D2")
      h <- length(unique(z$i)) * perception
      z$j <- cutree(hc, k=min(nrow(z), max(1, round(h))))
    }
  }

  if (condition == "event1") {
    z <- z[!duplicated(z$j) & !is.na(z$d),]
  }
  if (condition == "det1") {
    z <- z[!is.na(z$d),]
    z <- z[!duplicated(z$j),]
  }
  if (condition == "alldet") {
    z <- z[!is.na(z$d),]
    ## no filtering duplicated $j
  }
  ## angle in degrees counter clockwise from x axis right
  #z$a <- 180 * atan2(z$x, z$y) / pi
  #z$a[z$a < 0] <- 360+z$a[z$a < 0]
  ## observer position
  attr(z, "observer") <- x$xy
  attr(z, "condition") <- condition
  attr(z, "event_type") <- event_type
  attr(z, "tlim") <- tlim
  attr(z, "perception") <- perception
  attr(z, "n") <- length(x$events)
  class(z) <- c("bsims_detections_table", "data.frame")
  z
}
