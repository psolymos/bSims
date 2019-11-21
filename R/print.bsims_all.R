print.bsims_all <-
function(x, ...) {
    s <- x$settings()
    n <- nchar(names(s))
    m <- max(n)
    cat("bSims wrapper object with settings:\n")
    for (i in seq_along(s)) {
        cat("  ", names(s)[i], rep(" ", m-n[i]), ": ", 
			paste(s[[i]], collapse=", "), "\n", sep="")
    }
    invisible(x)
}
