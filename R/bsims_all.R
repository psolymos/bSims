## wrapper
## NULL arguments are dropped

## this returns the call
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

bsims_all <- function(...) {
  Settings <- list(...)
  if (length(Settings) == 1L && is.list(Settings[[1L]]))
    Settings <- Settings[[1L]]
  out <- list()
  out$settings <- function() Settings
  out$new <- function() .bsims_all(Settings)
  out$replicate <- function(B=1)  {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      pbreplicate(B, .bsims_all(Settings), simplify=FALSE)
    } else {
      replicate(B, .bsims_all(Settings), simplify=FALSE)
    }
  }
  out
}
