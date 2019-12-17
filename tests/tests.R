#remotes::install_github("psolymos/bSims")

cat("\n\n---------- Parsing Shiny apps:", "----------\n\n")
files <- list.files(system.file("shiny", package="bSims"),
                    pattern="\\.R$")
for (file in files) {
  cat(" * file:", file)
  tmp <- parse(file.path(
    system.file("shiny", package="bSims"), file))
  cat(" -- OK\n")
}
