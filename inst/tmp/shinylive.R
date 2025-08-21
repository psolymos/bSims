# clean up output folder
unlink("apps", recursive = TRUE)

# make shinylive apps
for (app in c("bsimsH", "bsimsHER", "distfunH", "distfunHER")) {
    # unlink(sprintf("apps/shiny/%s", app))
    # unlink(sprintf("apps/%s", app))
    dir.create(sprintf("apps/shiny/%s/www", app), recursive = TRUE, showWarnings = FALSE)
    file.copy(sprintf("inst/shiny/%s.R", app), sprintf("apps/shiny/%s/app.R", app), overwrite = TRUE)
    file.copy("inst/shiny/www/clipboard.min.js", sprintf("apps/shiny/%s/www/clipboard.min.js", app), overwrite = TRUE)
    shinylive::export(sprintf("apps/shiny/%s", app), sprintf("apps/%s", app))
}

# clean up
unlink("apps/shiny", recursive = TRUE)

# check shinylive apps
# httpuv::runStaticServer("apps/bsimsH")
