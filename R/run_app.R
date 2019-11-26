run_app <-
function(app=c("bsimsH", "bsimsHER", "distfunH", "distfunHER")) {
  shiny::runApp(
    system.file(paste0("shiny/", match.arg(app), ".R"), package="bSims"),
    display.mode = "normal")
}
