get_nests <- function (x, ...)
  UseMethod("get_nests")
get_nests.bsims_population <- function (x, ...)
  x$nests[,c("x", "y")]

get_events <- function (x, ...)
  UseMethod("get_events")
get_events.bsims_events <- function (x, ...) {
  out <- .get_events(x, ...)
  out$d <- NULL
  out
}

get_detections <- function (x, ...)
  UseMethod("get_detections")
get_detections.bsims_detections <- function (x, ...)
  .get_detections(x, ...)

get_abundance <- function (x, ...)
  UseMethod("get_abundance")
get_abundance.bsims_population <- function (x, ...)
  sum(x$abundance)

get_density <- function (x, ...)
  UseMethod("get_density")
get_density.bsims_population <- function (x, ...)
  sum(x$abundance) / x$extent^2
