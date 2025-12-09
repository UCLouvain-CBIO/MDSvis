# MDSvis - Copyright (C) <2025-2026>
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


#' @export
#' @title Launch shiny app for MDS projection visualization
#' @param preLoadDemoDataset if TRUE, pre-load the *Krieg_Anti_PD_1* dataset
#' @import shiny
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom CytoMDS nDim ggplotSampleMDS
#' @importFrom ggplot2 facet_wrap ggsave labs
#' @return no return value
#' @examples
#' if (interactive()) {
#'   mdsvis_app()
#' }
mdsvis_app <- function(preLoadDemoDataset = FALSE) {
    ui <- fluidPage(
        shinyjs::useShinyjs(),
        titlePanel("Plot of Metric MDS object"),
        tabsetPanel(
            id = "tabs",
            tabPanel("Input files",
                     sidebarLayout(
                         sidebarPanel(
                             fileInput("mdsObjFile", 
                                       "Choose MDS object file",
                                       accept = ".rds"),
                             fileInput("pDataFile", 
                                       "Choose phenodata file",
                                       accept = ".rds"),
                             uiOutput("pDataVariableSelector"),
                             fileInput("statsFile", 
                                       "Choose stats file",
                                       accept = ".rds")
                         ),
                         mainPanel()
                     )
            ),
            tabPanel("View",
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("axis1", 
                                         "X-axis coordinate:", 
                                         choices = NULL),
                             selectInput("axis2", 
                                         "Y-axis coordinate:", 
                                         choices = NULL),
                             selectInput("colourBy", 
                                         "Colour by:", 
                                         choices = NULL),
                             selectInput("labelBy", 
                                         "Label by:", 
                                         choices = NULL),
                             selectInput("shapeBy", 
                                         "Shape by:", 
                                         choices = NULL),
                             selectInput("facetBy", 
                                         "Facet by:", 
                                         choices = NULL),
                             checkboxInput("biplot", 
                                           "Show biplot", 
                                           value = FALSE),
                             conditionalPanel(
                                 condition = "input.biplot == true",
                                 selectInput("extVariables", 
                                             "Select statistic:",
                                             choices = NULL),
                                 numericInput("arrowThreshold", 
                                              "Arrow length threshold",
                                              value = 0.8, 
                                              step = 0.1)
                             ),
                             checkboxInput("plotlytooltipping", 
                                           "Show plotly plot",
                                           value = FALSE),
                             conditionalPanel(
                                 condition = "input.plotlytooltipping == true",
                                 selectInput("pDataForAdditionalLabelling",
                                             "Additional plotly tooltips:",
                                             choices = NULL, 
                                             multiple = TRUE)
                             ),
                             checkboxInput("flipXAxis", 
                                           "Flip X-axis", 
                                           value = FALSE),
                             checkboxInput("flipYAxis", 
                                           "Flip Y-axis", 
                                           value = FALSE),
                             actionButton("exportPlot", 
                                          "Export plot as PDF")
                         ),
                         mainPanel(
                             plotOutput("mdsPlot"),
                             plotly::plotlyOutput("mdsPlotly")
                         )
                     )
            ),
            tabPanel("General Settings",
                     sidebarLayout(
                         sidebarPanel(
                             numericInput("pointLabelSize", 
                                          "Point label size",
                                          value = 3, step = 0.1),
                             checkboxInput("repelPointLabels", 
                                           "Repel overlapping labels",
                                           value = TRUE),
                             conditionalPanel(
                                 condition = "input.repelPointLabels == true",
                                 numericInput("maxOverlaps", 
                                              "Maximum overlaps", 
                                              value = 10,
                                              step = 1)
                             ),
                             checkboxInput("displayPseudoRSq", 
                                           "Display pseudo RSquare",
                                           value = TRUE),
                             checkboxInput("pointSizeReflectingStress",
                                           "Point size reflecting stress", 
                                           value = FALSE),
                             conditionalPanel(
                                 condition = 
                                     "input.pointSizeReflectingStress == false",
                                 numericInput("pointSize", 
                                              "Point size", 
                                              value = 1,
                                              step = 0.1)
                             ),
                             checkboxInput("displayArrowLabels", 
                                           "Display arrow labels",
                                           value = TRUE),
                             conditionalPanel(
                                 condition = "input.displayArrowLabels == true",
                                 numericInput("arrowLabelSize", 
                                              "Arrow label size",
                                              value = 3, step = 0.1),
                                 checkboxInput("repelArrowLabels", 
                                               "Repel arrow labels",
                                               value = FALSE)
                             )
                         ),
                         mainPanel()
                     )
            ),
            selected = ifelse(preLoadDemoDataset, "View", "Input files")
        )
    )
    
    server <- function(input, output, session) {
        
        createMDSObjectFromFile <- function(mdsObjFilePath){
            mds <- NULL
            tryCatch({
                mds <- readRDS(mdsObjFilePath)
                if (!inherits(mds, "MDS")) {
                    rvIsMdsObjValid(FALSE)
                    stop("The selected file does not contain a MDS object.")
                }
                rvIsMdsObjValid(TRUE)
                
                updateSelectInput(session, "axis1",
                                  choices = seq_len(CytoMDS::nDim(mds)),
                                  selected = 1)
                updateSelectInput(session, "axis2",
                                  choices = seq_len(CytoMDS::nDim(mds)),
                                  selected = 2)
            }, error = function(e) {
                showNotification(as.character(e$message), 
                                 type = "error", 
                                 duration = NULL)
            })
            
            mds
        }
        
        createPDataFromFile <- function(pDataFilePath){
            
            pdata <- NULL
            tryCatch({
                pdata <- readRDS(pDataFilePath)
                
                if (!is.data.frame(pdata)) {
                    rvIsPDataValid(FALSE)
                    rvPData(NULL)
                    rvPDataSubs(NULL)
                    stop("The selected file does not contain ",
                         "a data.frame object.")
                }
                
                rvIsPDataValid(TRUE)
                
                updateSelectInput(session, "colourBy",
                                  choices = c(
                                      "_", 
                                      colnames(pdata)
                                      [vapply(pdata,
                                              FUN = function(x){
                                                  is.factor(x) || 
                                                      is.numeric(x) ||
                                                      is.logical(x)
                                              },
                                              FUN.VALUE = logical(1))]),
                                  selected = "_")
                updateSelectInput(session, "labelBy",
                                  choices = c(
                                      "_", 
                                      colnames(pdata)),
                                  selected = "_")
                updateSelectInput(session, "shapeBy",
                                  choices = c(
                                      "_",
                                      colnames(pdata)
                                      [vapply(
                                          pdata,
                                          FUN = function(x){
                                              is.factor(x) && 
                                                  length(levels(x)) <= 6
                                          },
                                          FUN.VALUE = logical(1))]),
                                  selected = "_")
                updateSelectInput(session, "facetBy",
                                  choices = c(
                                      "_", 
                                      colnames(pdata)
                                      [vapply(
                                          pdata,
                                          FUN = is.factor,
                                          FUN.VALUE = logical(1))]),
                                  selected = "_")
                updateSelectInput(session, "pDataForAdditionalLabelling",
                                  choices = colnames(pdata))
                
                
                
            }, error = function(e) {
                showNotification(as.character(e$message), 
                                 type = "error", 
                                 duration = NULL)
            })
            
            pdata
        }
        
        createStatsFromFile <- function(statsFilePath) {
            stats <- NULL
            tryCatch({
                stats <- readRDS(statsFilePath)
                if (!(is.list(stats) && all(vapply(stats, 
                                                   FUN = is.matrix,
                                                   FUN.VALUE = logical(1))))) {
                    rvIsStatsValid(FALSE)
                    updateSelectInput(session, 
                                      "extVariables", 
                                      choices = c("_"))
                    stop("The selected file does not contain ",
                         "a list of matrices")
                }
                
                rvIsStatsValid(TRUE)
                
                updateSelectInput(session, "extVariables", 
                                  choices = names(stats),
                                  selected = names(stats)[1])
                
            }, error = function(e) {
                showNotification(as.character(e$message), 
                                 type = "error", 
                                 duration = NULL)
            })
            stats
        }
        
        checkInputObjectDims <- function(mds, pdata, stats) {
            dimCheck <- checkDimCompatibility(
                mds, pdata, stats)
            
            tryCatch({
                
                notNAdims <- dimCheck$dims[!is.na(dimCheck$dims)]
                if(length(unique(notNAdims)) > 1) {
                    stop("The loaded objects have incompatible ",
                         "number of samples (",
                         paste(
                             vapply(
                                 seq_along(notNAdims),
                                 FUN = function(i) paste(notNAdims[i],
                                                         names(notNAdims)[i],
                                                         sep = ": "),
                                 FUN.VALUE = character(1)),
                             collapse = ", "),
                         ")")
                }
            }, error = function(e) {
                showNotification(as.character(e$message), 
                                 type = "error", 
                                 duration = NULL)
            })
            
            dimCheck
        }
        
        
        # Increase the maximum upload size to 50 MB
        options(shiny.maxRequestSize = 50 * 1024^2)
        
        rvIsMdsObjValid <- reactiveVal(TRUE)
        rvIsPDataValid <- reactiveVal(TRUE)
        rvIsStatsValid <- reactiveVal(TRUE)
        rvAreMDSPdataCompatible <- reactiveVal(TRUE)
        rvAreMDSStatsCompatible <- reactiveVal(TRUE)
        
        
        if (preLoadDemoDataset) {
            initDataPath <- system.file("extdata", package = "MDSvis")
            initMDSFilePath <- file.path(initDataPath,
                                         "Krieg_mdsObj.rds")
            initPDataFilePath <- file.path(initDataPath,
                                           "Krieg_phenoData.rds")
            initStatFilePath <- file.path(initDataPath,
                                          "Krieg_chStats.rds")
            rvMdsObj <- reactiveVal(
                createMDSObjectFromFile(
                    mdsObjFilePath = initMDSFilePath)
            )
            
            myPData <- createPDataFromFile(initPDataFilePath)
            rvPData <- reactiveVal(myPData)      
            rvPDataSubs <- reactiveVal(myPData)
            
            rvStats <- reactiveVal(
                createStatsFromFile(initStatFilePath)
            )
        } else {
            rvMdsObj <- reactiveVal(NULL)
            rvPData <- reactiveVal(NULL)
            rvPDataSubs <- reactiveVal(NULL)
            rvStats <- reactiveVal(NULL)
        }
        
        observe({
            if (!rvIsMdsObjValid()) {
            #if (is.null(input$mdsObjFile) || !rvIsMdsObjValid()) {
                shinyjs::disable("axis1")
                shinyjs::disable("axis2")
                shinyjs::disable("colourBy")
                shinyjs::disable("labelBy")
                shinyjs::disable("facetBy")
                shinyjs::disable("shapeBy")
                shinyjs::disable("biplot")
                shinyjs::disable("plotlytooltipping")
                shinyjs::disable("flipXAxis")
                shinyjs::disable("flipYAxis")
                shinyjs::disable("pointLabelSize")
                shinyjs::disable("displayPointLabels")
                shinyjs::disable("repelPointLabels")
                shinyjs::disable("displayPseudoRSq")
                shinyjs::disable("pointSize")
                shinyjs::disable("pointSizeReflectingStress")
                shinyjs::disable("displayArrowLabels")
                shinyjs::disable("arrowLabelSize")
                shinyjs::disable("repelArrowLabels")
                shinyjs::disable("maxOverlaps")
                shinyjs::disable("exportPlot")
                if (input$repelPointLabels) {
                    shinyjs::disable("maxOverlaps")
                }
                if (!input$pointSizeReflectingStress) {
                    shinyjs::disable("pointSize")
                }
                if (input$displayArrowLabels) {
                    shinyjs::disable("arrowLabelSize")
                    shinyjs::disable("repelArrowLabels")
                } 
            } 
            if (rvIsMdsObjValid()){
            #if (!is.null(input$mdsObjFile) && rvIsMdsObjValid()){
                shinyjs::enable("axis1")
                shinyjs::enable("axis2")
                shinyjs::enable("flipXAxis")
                shinyjs::enable("flipYAxis")
                shinyjs::enable("pointLabelSize")
                shinyjs::enable("displayPointLabels")
                shinyjs::enable("repelPointLabels")
                shinyjs::enable("displayPseudoRSq")
                shinyjs::enable("pointSize")
                shinyjs::enable("pointSizeReflectingStress")
                shinyjs::enable("plotlytooltipping")
                shinyjs::enable("maxOverlaps")
                shinyjs::enable("exportPlot")
                if (input$repelPointLabels) {
                    shinyjs::enable("maxOverlaps")
                }
                if (!input$pointSizeReflectingStress) {
                    shinyjs::enable("pointSize")
                }
                if (input$displayArrowLabels) {
                    shinyjs::enable("arrowLabelSize")
                    shinyjs::enable("repelArrowLabels")
                } 
                if (!is.null(rvStats()) && rvIsStatsValid() && 
                    rvAreMDSStatsCompatible()) {
                    shinyjs::enable("biplot")
                    shinyjs::enable("displayArrowLabels")
                    shinyjs::enable("arrowLabelSize")
                    shinyjs::enable("repelArrowLabels")
                    if (input$biplot) {
                        shinyjs::enable("extVariables")
                        shinyjs::enable("arrowThreshold")
                    }
                } else {
                    shinyjs::disable("biplot")
                    shinyjs::disable("displayArrowLabels")
                    shinyjs::disable("arrowLabelSize")
                    shinyjs::disable("repelArrowLabels")
                    if (input$biplot) {
                        shinyjs::disable("extVariables")
                        shinyjs::disable("arrowThreshold")
                    }
                }
                if (!is.null(rvPData()) && rvIsPDataValid() && 
                    rvAreMDSPdataCompatible()){
                    shinyjs::enable("colourBy")
                    shinyjs::enable("labelBy")
                    shinyjs::enable("shapeBy")
                    shinyjs::enable("facetBy")
                    if(input$plotlytooltipping) {
                        shinyjs::enable("pDataForAdditionalLabelling")
                    }
                } else {
                    shinyjs::disable("colourBy")
                    shinyjs::disable("labelBy")
                    shinyjs::disable("shapeBy")
                    shinyjs::disable("facetBy")
                    if(input$plotlytooltipping) {
                        shinyjs::disable("pDataForAdditionalLabelling")
                    }
                }
            }
        })
        
        observeEvent(input$mdsObjFile, {
            req(input$mdsObjFile)
            mds <- createMDSObjectFromFile(
                mdsObjFilePath = input$mdsObjFile$datapath)
            
            rvMdsObj(mds)
            
            dimCheck <- checkInputObjectDims(
                rvMdsObj(), rvPData(), rvStats())
            
            rvAreMDSPdataCompatible(
                dimCheck$dimCompatibility$areMDSPdataCompatible)
            rvAreMDSStatsCompatible(
                dimCheck$dimCompatibility$areMDSStatsCompatible)
        })
        
        observeEvent(input$pDataFile, {
            req(input$pDataFile)
            pdata <- createPDataFromFile(
                pDataFilePath = input$pDataFile$datapath)
            rvPData(pdata)
            rvPDataSubs(pdata)

            dimCheck <- checkInputObjectDims(
                rvMdsObj(), rvPData(), rvStats())

            rvAreMDSPdataCompatible(
                dimCheck$dimCompatibility$areMDSPdataCompatible)
        })
        
        observeEvent(input$statsFile, {
            req(input$statsFile)
            stats <- createStatsFromFile(
                statsFilePath = input$statsFile$datapath)
            rvStats(stats)
            
            dimCheck <- checkInputObjectDims(
                rvMdsObj(), rvPData(), rvStats())
            
            rvAreMDSStatsCompatible(
                dimCheck$dimCompatibility$areMDSStatsCompatible)
        })
        
        
        p <- reactive({
            if (!is.null(rvMdsObj()) && rvIsMdsObjValid() && 
                input$axis1 != "" && input$axis2 != "") {
                plotargs <- list(
                    mdsObj = rvMdsObj(),
                    projectionAxes = c(as.integer(input$axis1), 
                                       as.integer(input$axis2)),
                    flipXAxis = input$flipXAxis,
                    flipYAxis = input$flipYAxis,
                    pointLabelSize = input$pointLabelSize,
                    displayPointLabels = FALSE,
                    repelPointLabels = input$repelPointLabels,
                    displayPseudoRSq = input$displayPseudoRSq,
                    pointSizeReflectingStress = input$pointSizeReflectingStress,
                    pointSize = input$pointSize
                )
                if (!is.null(input$maxOverlaps)) {
                    plotargs$max.overlaps <- input$maxOverlaps
                }
                if (!is.null(rvStats()) && rvIsStatsValid() && 
                    rvAreMDSStatsCompatible()) {
                    plotargs$biplot <- input$biplot
                    plotargs$extVariables <- rvStats()[[input$extVariables]]
                    plotargs$displayArrowLabels <- input$displayArrowLabels
                    plotargs$arrowLabelSize <- input$arrowLabelSize
                    plotargs$repelArrowLabels <- input$repelArrowLabels
                    plotargs$arrowThreshold <- input$arrowThreshold
                }
                if (!is.null(rvPData()) && rvIsPDataValid() && 
                    rvAreMDSPdataCompatible()) {
                    plotargs$pData <- rvPDataSubs()
                    if (input$colourBy != "_") {
                        plotargs$pDataForColour <- input$colourBy
                    }
                    if (input$labelBy != "_") {
                        plotargs$pDataForLabel <- input$labelBy
                        plotargs$displayPointLabels <- TRUE
                    }
                    if (input$shapeBy != "_") {
                        plotargs$pDataForShape <- input$shapeBy
                    }
                }
                if (length(input$pDataForAdditionalLabelling)) {
                    plotargs$pDataForAdditionalLabelling <- 
                        input$pDataForAdditionalLabelling
                }
                pltargs <- 
                    plotargs[!(names(plotargs) 
                               %in% "pDataForAdditionalLabelling")]
                pltlyargs <- plotargs[!(names(plotargs) %in% "biplot")]
                pltlyargs$repelPointLabels <- FALSE
                plt <- do.call(CytoMDS::ggplotSampleMDS, pltargs)
                pltly <- do.call(CytoMDS::ggplotSampleMDS, pltlyargs)
                if (!is.null(rvPData()) && rvIsPDataValid() && 
                    rvAreMDSPdataCompatible() && input$facetBy != "_") {
                    plt <- plt + ggplot2::facet_wrap(~ .data[[input$facetBy]])
                    pltly <- pltly + 
                        ggplot2::facet_wrap(~ .data[[input$facetBy]])
                }
                list(plt = plt,
                     pltly = pltly)
            }
        })
        
        output$mdsPlot <- renderPlot({
            req(rvMdsObj())
            p()$plt
        })
        
        output$mdsPlotly <- plotly::renderPlotly({
            req(rvMdsObj())
            req(input$plotlytooltipping)
            plotly::ggplotly(p()$pltly)
        })
        
        output$pDataVariableSelector <- renderUI({
            req(rvPData())
            selectInput("pDataVariableSelection",
                        "Select phenodata variables to include:",
                        choices = colnames(rvPData()),
                        selected = colnames(rvPData()),
                        multiple = TRUE)
        })
        
        observeEvent(input$pDataVariableSelection, {
            req(rvPData(), input$pDataVariableSelection)
            rvPDataSubs(rvPData()[, input$pDataVariableSelection, drop = FALSE])
            updateSelectInput(session, "colourBy",
                              choices = c(
                                  "_", 
                                  colnames(rvPDataSubs())[
                                      vapply(
                                          rvPDataSubs(),
                                          FUN = function(x){
                                              is.factor(x) || 
                                                  is.numeric(x) ||
                                                  is.logical(x)
                                          },
                                          FUN.VALUE = logical(1))]),
                              selected = "_")
            updateSelectInput(session, "labelBy",
                              choices = c("_", 
                                          colnames(rvPDataSubs())),
                              selected = "_")
            updateSelectInput(session, "shapeBy",
                              choices = c(
                                  "_",
                                  colnames(rvPDataSubs())
                                  [vapply(
                                      rvPDataSubs(),
                                      FUN = function(x){
                                          is.factor(x) && 
                                              length(levels(x)) <= 6
                                      },
                                      FUN.VALUE = logical(1))]),
                              selected = "_")
            updateSelectInput(session, "facetBy",
                              choices = c(
                                  "_",
                                  colnames(rvPDataSubs())
                                  [vapply(
                                      rvPDataSubs(),
                                      FUN = is.factor,
                                      FUN.VALUE = logical(1))]),
                              selected = "_")
            updateSelectInput(session, "pDataForAdditionalLabelling",
                              choices = colnames(rvPDataSubs()))
        })
        
        observeEvent(input$exportPlot, {
            showModal(modalDialog(
                title = "Export Settings",
                textInput("plot_title", 
                          "Plot title", 
                          value = "Multi Dimensional Scaling"),
                numericInput("plot_width", 
                             "Width (inches)", 
                             value = 7, 
                             min = 1),
                numericInput("plot_height", "Height (inches)", 
                             value = 5, 
                             min = 1),
                downloadButton("confirmDownload", 
                               "Download PDF"),
                easyClose = TRUE
            ))
        })
        
        output$confirmDownload <- downloadHandler(
            filename = function() { "Multi Dimensional Scaling.pdf" },
            
            content = function(file) {
                
                removeModal()
                
                title <- input$plot_title
                width <- input$plot_width
                height <- input$plot_height
                
                validate(
                    need(!is.null(width) && !is.null(height), 
                         "Width and height must be specified.")
                )
                
                ggplot2::ggsave(file, 
                                plot = p()$plt + ggplot2::labs(title = title), 
                                device = "pdf", 
                                width = width, 
                                height = height)
            }
        )
        
    }
    
    shinyApp(ui = ui, server = server)
}
