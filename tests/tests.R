#remotes::install_github("psolymos/bSims")
## examples

library(bSims)

help_pages <- c(
    "acceptreject",
    "bsims_init",
    "dist_fun2",
    "events",
    #"get_events",
    #"plot.bsims_landscape",
    "rlnorm2",
    "rmvn")

for (i in help_pages) {
    cat("\n\n---------- bSims example:", i, "----------\n\n")
    eval(parse(text=paste0("example('", i,
        "', package = 'bSims', run.dontrun = TRUE)")))
}

cat("\n\n---------- Parsing Shiny apps:", "----------\n\n")
files <- list.files(system.file("shiny", package="bSims"), pattern="\\.R$")
for (file in files) {
    cat(" * file:", file)
    tmp <- parse(file.path(
        system.file("shiny", package="bSims"), file))
    cat(" -- OK\n")
}
