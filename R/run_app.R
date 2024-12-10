#' Launch shiny app for MDS visualization
#'
#' @export
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom CytoMDS nDim ggplotSampleMDS
#' @importFrom shiny runApp
run_app <- function() {
  runApp(system.file("app/app.R", package = "MDSgui"))
}
