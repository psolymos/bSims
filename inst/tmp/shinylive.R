# bsimsH

for (app in c("bsimsH", "bsimsHER", "distfunH", "distfunHER")) {

message(app)

dir.create(sprintf("apps/shiny/%s/www", app), recursive = TRUE, showWarnings = FALSE)
file.copy(sprintf("inst/shiny/%s.R", app), sprintf("apps/shiny/%s/app.R", app), overwrite = TRUE)
file.copy("inst/shiny/www/clipboard.min.js", sprintf("apps/shiny/%s/www/clipboard.min.js", app), overwrite = TRUE)
shinylive::export(sprintf("apps/shiny/%s", app), sprintf("apps/%s", app))

}
unlink("apps/shiny", recursive = TRUE)

# httpuv::runStaticServer("_temp/shinylive/distfunH")
