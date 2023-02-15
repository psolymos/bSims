## wrapper
## NULL arguments are dropped

## internal function
.bsims_all <- function(Settings) {
  Functions <- list(
    bsims_init=bsims_init,
    bsims_populate=bsims_populate,
    bsims_animate=bsims_animate,
    bsims_detect=bsims_detect,
    bsims_transcribe=bsims_transcribe)
  Formals <- lapply(Functions, formals)
  Formals <- lapply(Formals, function(z) z[names(z) != "..."])
  Formals <- lapply(Formals, function(z) z[names(z) != "x"])
  for (i in seq_len(length(Functions))) {
    Call <- if (i == 1L)
      Functions[[i]]()$call else Functions[[i]](Last)$call
    Call[[1L]] <- as.name(names(Functions)[i])
    if (i > 1L)
      Call[["x"]] <- as.name("Last")
    for (j in names(Settings)) {
      if (j %in% names(Formals[[i]])) {
        Formals[[i]][[j]] <- Settings[[j]]
        if (!is.null(Settings[[j]]))
          Call[[j]] <- Settings[[j]]
      }
    }
    Last <- eval(Call)
  }
  Last
}

# list all arguments
.bsims_args <- function() {
  Functions <- list(
    bsims_init=bsims_init,
    bsims_populate=bsims_populate,
    bsims_animate=bsims_animate,
    bsims_detect=bsims_detect,
    bsims_transcribe=bsims_transcribe)
  Formals <- lapply(Functions, formals)
  Formals <- lapply(Formals, function(z) z[names(z) != "..."])
  Formals <- lapply(Formals, function(z) z[names(z) != "x"])
  out <- NULL
  for (i in Formals)
    out <- c(out, i)
  unique(names(out))
}

bsims_all <- function(...) {
  Settings <- list(...)
  if (length(Settings) == 1L && is.list(Settings[[1L]]))
    Settings <- Settings[[1L]]
  if (is.null(names(Settings)))
    stop("Settings must be a named list.")
  if (any(duplicated(names(Settings))))
    stop("Duplicate arguments are not allowed.")
  Settings <- Settings[names(Settings) %in% .bsims_args()]
  out <- list()
  out$settings <- function() Settings
  out$new <- function(recover=FALSE) {
    if (recover)
      try(.bsims_all(Settings)) else .bsims_all(Settings)
  }
  out$replicate <- function(B=1, recover=FALSE, cl=NULL)  {
    if (!is.null(cl) && inherits(cl, "cluster")) {
      isLoaded <- all(unlist(clusterEvalQ(cl, "bSims" %in% .packages())))
      if (!isLoaded)
        clusterEvalQ(cl, library(bSims))
      clusterExport(cl, c(".bsims_all"))
    }
    z <- if (recover) {
      pbreplicate(B, try(.bsims_all(Settings)), simplify=FALSE, cl=cl)
    } else {
      pbreplicate(B, .bsims_all(Settings), simplify=FALSE, cl=cl)
    }
    if (!is.null(cl) && inherits(cl, "cluster")) {
      clusterEvalQ(cl, rm(.bsims_all))
      if (!isLoaded)
        clusterEvalQ(cl, detach("package:bSims", unload=TRUE))
    }
    z
  }
  class(out) <- c("bsims_all",
                "bsims_transcript",
                "bsims_detections",
                "bsims_events",
                "bsims_population",
                "bsims_landscape",
                "bsims")
  out
}
