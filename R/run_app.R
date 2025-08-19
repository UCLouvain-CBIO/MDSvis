# MDSvis - Copyright (C) <2025>
# <Université catholique de Louvain (UCLouvain), Belgique>
#
#   Description and complete License: see LICENSE file.
#
# This program (MDSvis) is free software:
#   you can redistribute it and/or modify it under the terms of the GNU General
# Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details (<http://www.gnu.org/licenses/>).
#

#' Launch shiny app for MDS projection visualization
#'
#' @export
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom CytoMDS nDim ggplotSampleMDS
#' @importFrom shiny runApp
#' @return no return value
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function() {
    runApp(system.file("app/app.R", package = "MDSvis"))
}
