## condition for being available:
## "event1" 1st event (move or vocal)
## "det1" 1st detection (inflates time-to-detection)
## "alldet" count all detections (double counting)
get_detections <-
function(x,
  condition=c("event1", "det1", "alldet"),
  event_type=c("vocal", "move", "both"), tlim=NULL)
{
  condition <- match.arg(condition)
  event_type <- match.arg(event_type)
  ## get the events
  z <- get_events(x, event_type=event_type, tlim=tlim)
  if (nrow(z) == 0) {
    z$d <- numeric(0)
    return(z)
  }
  if (condition == "event1") {
    z <- z[!duplicated(z$i) & !is.na(z$d),]
  }
  if (condition == "det1") {
    z <- z[!is.na(z$d),]
    z <- z[!duplicated(z$i),]
  }
  if (condition == "alldet") {
    z <- z[!is.na(z$d),]
  }
  ## angle in degrees counter clockwise from x axis right
  #z$a <- 180 * atan2(z$x, z$y) / pi
  #z$a[z$a < 0] <- 360+z$a[z$a < 0]
  ## observer position
  attr(z, "observer") <- x$xy
  attr(z, "condition") <- condition
  attr(z, "event_type") <- event_type
  attr(z, "tlim") <- tlim
  z
}
