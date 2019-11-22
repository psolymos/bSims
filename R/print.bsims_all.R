print.bsims_all <-
function(x, ...) {
    s <- x$settings()
    n <- nchar(names(s))
    m <- max(n)
    cat("bSims wrapper object with settings:\n")
    for (i in seq_along(s)) {
        if (is.null(s[[i]])) {
            val <- "NULL"
        } else {
            if (is.function(s[[i]])) {
                val <- paste(deparse(s[[i]]), collapse="")
                if (nchar(val) > 50)
                    val <- paste0(substr(val, 1, 50), " [...]")
            } else {
                val <- paste(s[[i]], collapse=", ")
            }
        }
        cat("  ", names(s)[i], rep(" ", m-n[i]), ": ",
            val, "\n", sep="")
    }
    invisible(x)
}
