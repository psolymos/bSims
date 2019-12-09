get_nests <- function (x, ...)
  UseMethod("get_nests")
get_nests.bsims_population <- function (x, ...) {
  out <- x$nests
  ## behav groups not defined for pop object
  if (is.null(out$g))
    out$g <- NA
  out
}

get_abundance <- function (x, ...)
  UseMethod("get_abundance")
get_abundance.bsims_population <- function (x, ...)
  sum(x$abundance)

get_density <- function (x, ...)
  UseMethod("get_density")
get_density.bsims_population <- function (x, ...)
  sum(x$abundance) / x$extent^2

get_events <- function (x, ...)
  UseMethod("get_events")
get_events.bsims_events <- function (x, ...) {
  ## event type is not defined for events object
  event_type <- if (is.null(x$event_type))
    "both" else x$event_type
  out <- .get_events(x, event_type=event_type, ...)
  ## remove true distances
  out$d <- NULL
  out
}

get_detections <- function (x, ...)
  UseMethod("get_detections")
get_detections.bsims_detections <- function (x, ...) {
  ## condition is not defined for detection object
  condition <- if (is.null(x$condition))
    "alldet" else x$condition
  .get_detections(x,
    event_type=x$event_type,
    condition=condition,
    perception=x$perception, ...)
}

get_table <- function (x, ...)
  UseMethod("get_table")
get_table.bsims_transcript <- function(x, type=c("removal", "visits"), ...)
  switch(match.arg(type),
    "removal"=x$removal,
    "visits"=x$visits)
