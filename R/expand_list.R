expand_list <-
function(...) {
  a <- list(...)
  if (is.null(names(a)) || any(names(a) == ""))
    stop("all arguments must be named")
  ## expand.grid call with indices
  v <- call("expand.grid")
  for (i in seq_along(a)) {
    v[[names(a)[i]]] <- seq_along(a[[i]])
  }
  d <- eval(v)
  ## output list
  out <- list()
  for (j in seq_len(nrow(d))) {
    l <- list()
    for (k in seq_len(ncol(d))) {
      h <- colnames(d)[k]
      if (is.null(a[[h]][[d[j,h]]])) {
        l[[h]] <- "NULL"
        l[h] <- list(NULL)
      } else {
        l[[h]] <- a[[h]][[d[j,h]]]
      }
    }
    out[[j]] <- l
  }
  out
}
