#' Launch shiny app for MDS projection visualization
#'
#' @export
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom CytoMDS nDim ggplotSampleMDS
#' @importFrom shiny runApp
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function() {
  runApp(system.file("app/app.R", package = "MDSvis"))
}
