ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Plot of Metric MDS object"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Input files",
             sidebarLayout(
               sidebarPanel(
                 fileInput("mdsObjFile", "Choose MDS object file",
                           accept = ".rds"),
                 fileInput("pDataFile", "Choose phenodata file",
                           accept = ".rds"),
                 uiOutput("pDataVariableSelector"),
                 fileInput("statsFile", "Choose stats file",
                           accept = ".rds")
               ),
               mainPanel()
             )
    ),
    tabPanel("View",
             sidebarLayout(
               sidebarPanel(
                 selectInput("axis1", "X-axis coordinate:", choices = NULL),
                 selectInput("axis2", "Y-axis coordinate:", choices = NULL),
                 selectInput("colourBy", "Colour by:", choices = NULL),
                 selectInput("labelBy", "Label by:", choices = NULL),
                 selectInput("shapeBy", "Shape by:", choices = NULL),
                 selectInput("facetBy", "Facet by:", choices = NULL),
                 checkboxInput("biplot", "Show biplot", value = FALSE),
                 conditionalPanel(
                   condition = "input.biplot == true",
                   selectInput("extVariables", "Select statistic:",
                               choices = NULL),
                   numericInput("arrowThreshold", "Arrow length threshold",
                                value = 0.8, step = 0.1)
                 ),
                 checkboxInput("plotlytooltipping", "Show plotly plot",
                               value = FALSE),
                 conditionalPanel(
                   condition = "input.plotlytooltipping == true",
                   selectInput("pDataForAdditionalLabelling",
                               "Additional plotly tooltips:",
                               choices = NULL, multiple = TRUE)
                 ),
                 checkboxInput("flipXAxis", "Flip X-axis", value = FALSE),
                 checkboxInput("flipYAxis", "Flip Y-axis", value = FALSE),
                 actionButton("exportPlot", "Export plot as PDF")
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
                 numericInput("pointLabelSize", "Point label size",
                              value = 3.88, step = 0.1),
                 checkboxInput("repelPointLabels", "Repel overlapping labels",
                               value = TRUE),
                 conditionalPanel(
                   condition = "input.repelPointLabels == true",
                   numericInput("maxOverlaps", "Maximum overlaps", value = 10,
                                step = 1)
                 ),
                 checkboxInput("displayPseudoRSq", "Display pseudo RSquare",
                               value = TRUE),
                 checkboxInput("pointSizeReflectingStress",
                               "Point size reflecting stress", value = FALSE),
                 conditionalPanel(
                   condition = "input.pointSizeReflectingStress == false",
                   numericInput("pointSize", "Point size", value = 1,
                                step = 0.1)
                 ),
                 checkboxInput("displayArrowLabels", "Display arrow labels",
                               value = TRUE),
                 conditionalPanel(
                   condition = "input.displayArrowLabels == true",
                   numericInput("arrowLabelSize", "Arrow label size",
                                value = 3.88, step = 0.1),
                   checkboxInput("repelArrowLabels", "Repel arrow labels",
                                 value = FALSE)
                 )
               ),
               mainPanel()
             )
    )
  )
)

server <- function(input, output, session) {

  # Increase the maximum upload size to 50 MB
  options(shiny.maxRequestSize = 50 * 1024^2)

  mdsObj <- reactiveVal(NULL)
  pData <- reactiveVal(NULL)
  pDataSubs <- reactiveVal(NULL)
  stats <- reactiveVal(NULL)
  
  isMdsObjValid <- reactiveVal(TRUE)
  isPDataValid <- reactiveVal(TRUE)
  isStatsValid <- reactiveVal(TRUE)
  areMDSPdataCompatible <- reactiveVal(TRUE)
  areMDSStatsCompatible <- reactiveVal(TRUE)
  
  observe({
    if (is.null(input$mdsObjFile) || !isMdsObjValid()) {
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
    if (!is.null(input$mdsObjFile) && isMdsObjValid()){
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
      if (!is.null(input$statsFile) && isStatsValid() && areMDSStatsCompatible()) {
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
      if (!is.null(input$pDataFile) && isPDataValid() && areMDSPdataCompatible()){
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
    tryCatch({
      mds <- readRDS(input$mdsObjFile$datapath)
      if (is(mds)[1] != "MDS") {
        isMdsObjValid(FALSE)
        stop("The selected file does not contain a MDS object.")
      }
      isMdsObjValid(TRUE)
      mdsObj(mds)
      updateSelectInput(session, "axis1",
                        choices = seq_len(CytoMDS::nDim(mdsObj())),
                        selected = 1)
      updateSelectInput(session, "axis2",
                        choices = seq_len(CytoMDS::nDim(mdsObj())),
                        selected = 2)
    }, error = function(e) {
      showNotification(as.character(e$message), type = "error", duration = NULL)
    })
  })

  observeEvent(input$pDataFile, {
    req(input$pDataFile)
    tryCatch({
      pdata <- readRDS(input$pDataFile$datapath)

      if (!is.data.frame(pdata)) {
        isPDataValid(FALSE)
        pData(NULL)
        pDataSubs(NULL)
        stop("The selected file does not contain a data.frame object.")
      }
      
      isPDataValid(TRUE)
      pData(pdata)
      pDataSubs(pdata)

      updateSelectInput(session, "colourBy",
                        choices = c("_", colnames(pDataSubs())),
                        selected = "_")
      updateSelectInput(session, "labelBy",
                        choices = c("_", colnames(pDataSubs())),
                        selected = "_")
      updateSelectInput(session, "shapeBy",
                        choices = c("_",
                                    colnames(pDataSubs())[sapply(pDataSubs(),
                                                                 is.factor)]),
                        selected = "_")
      updateSelectInput(session, "facetBy",
                        choices = c("_", 
                                    colnames(pDataSubs())[sapply(pDataSubs(),
                                                                 is.factor)]),
                        selected = "_")
      updateSelectInput(session, "pDataForAdditionalLabelling",
                        choices = colnames(pDataSubs()))

    }, error = function(e) {
      showNotification(as.character(e$message), type = "error", duration = NULL)
    })
  })

  observeEvent(input$statsFile, {
    req(input$statsFile)
    tryCatch({
      stats_ <- readRDS(input$statsFile$datapath)
      if (!(is.list(stats_) && all(sapply(stats_, is.matrix)))) {
        isStatsValid(FALSE)
        stats(NULL)
        updateSelectInput(session, "extVariables", choices = c("_"))
        stop("The selected file does not contain a list of matrices")
      }
      
      isStatsValid(TRUE)
      stats(stats_)
      updateSelectInput(session, "extVariables", choices = names(stats()),
                        selected = names(stats())[1])

    }, error = function(e) {
      showNotification(as.character(e$message), type = "error", duration = NULL)
    })
  })

  observeEvent(c(input$mdsObjFile, input$pDataFile, input$statsFile), {
    tryCatch({
      dims <- rep(NA, 3)
      names(dims) <- c("MDS", "phenodata", "stats")
      if (!is.null(input$mdsObjFile) && isMdsObjValid()) {
        dims[1] <- CytoMDS::nPoints(readRDS(input$mdsObjFile$datapath))
      }
      if (!is.null(input$pDataFile) && isPDataValid()) {
        dims[2] <- nrow(readRDS(input$pDataFile$datapath))
      }
      if (!is.null(input$statsFile) && isStatsValid()) {
        dims[3] <- nrow(readRDS(input$statsFile$datapath)[[1]])
      }
      if (!is.na(dims[1])) {
        if (!is.na(dims[2]) && dims[1] != dims[2])
          areMDSPdataCompatible(FALSE)
        if (!is.na(dims[3]) && dims[1] != dims[3])
          areMDSStatsCompatible(FALSE)
      }
      notNAdims <- dims[!is.na(dims)]
      if(length(unique(notNAdims)) > 1) {
        stop(paste0("The loaded objects have incompatible number of samples (",
                    paste(sapply(seq_along(notNAdims),
                                 function(i) paste(notNAdims[i],
                                                   names(notNAdims)[i],
                                                   sep = ": ")),
                          collapse = ", "),
                    ")"))
      }
      areMDSPdataCompatible(TRUE)
      areMDSStatsCompatible(TRUE)
    }, error = function(e) {
      showNotification(as.character(e$message), type = "error", duration = NULL)
    })
  })

  p <- reactive({
    if (isMdsObjValid()) {
      plotargs = list(
        mdsObj = mdsObj(),
        projectionAxes = c(as.integer(input$axis1), as.integer(input$axis2)),
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
        plotargs$max.overlaps = input$maxOverlaps
      }
      if (!is.null(input$statsFile) && isStatsValid() && areMDSStatsCompatible()) {
        plotargs$biplot = input$biplot
        plotargs$extVariables = stats()[[input$extVariables]]
        plotargs$displayArrowLabels = input$displayArrowLabels
        plotargs$arrowLabelSize = input$arrowLabelSize
        plotargs$repelArrowLabels = input$repelArrowLabels
        plotargs$arrowThreshold = input$arrowThreshold
      }
      if (!is.null(input$pDataFile) && isPDataValid() && areMDSPdataCompatible()) {
        plotargs$pData = pDataSubs()
        if (input$colourBy != "_") {
          plotargs$pDataForColour = input$colourBy
        }
        if (input$labelBy != "_") {
          plotargs$pDataForLabel = input$labelBy
          plotargs$displayPointLabels = TRUE
        }
        if (input$shapeBy != "_") {
          plotargs$pDataForShape = input$shapeBy
        }
      }
      if (length(input$pDataForAdditionalLabelling)) {
        plotargs$pDataForAdditionalLabelling = input$pDataForAdditionalLabelling
      }
      pltargs = plotargs[!(names(plotargs) %in% "pDataForAdditionalLabelling")]
      pltlyargs = plotargs[!(names(plotargs) %in% "biplot")]
      pltlyargs$repelPointLabels = FALSE
      plt = do.call(CytoMDS::ggplotSampleMDS, pltargs)
      pltly = do.call(CytoMDS::ggplotSampleMDS, pltlyargs)
      if (!is.null(input$pDataFile) && input$facetBy != "_") {
        plt = plt + ggplot2::facet_wrap(~ .data[[input$facetBy]])
        pltly = pltly + ggplot2::facet_wrap(~ .data[[input$facetBy]])
      }
      list(plt = plt,
           pltly = pltly)
    }
  })

  output$mdsPlot <- renderPlot({
    req(mdsObj())
    p()$plt
  })

  output$mdsPlotly <- plotly::renderPlotly({
    req(mdsObj())
    req(input$plotlytooltipping)
    plotly::ggplotly(p()$pltly)
  })

  output$pDataVariableSelector <- renderUI({
    req(pData())
    selectInput("pDataVariableSelection",
                "Select phenodata variables to include:",
                choices = colnames(pData()),
                selected = colnames(pData()),
                multiple = TRUE)
  })
  
  observeEvent(input$pDataVariableSelection, {
    req(pData(), input$pDataVariableSelection)
    pDataSubs(pData()[, input$pDataVariableSelection, drop = FALSE])
    updateSelectInput(session, "colourBy",
                      choices = c("_", colnames(pDataSubs())),
                      selected = "_")
    updateSelectInput(session, "labelBy",
                      choices = c("_", colnames(pDataSubs())),
                      selected = "_")
    updateSelectInput(session, "shapeBy",
                      choices = c("_",
                                  colnames(pDataSubs())[sapply(pDataSubs(),
                                                               is.factor)]),
                      selected = "_")
    updateSelectInput(session, "facetBy",
                      choices = c("_",
                                  colnames(pDataSubs())[sapply(pDataSubs(),
                                                               is.factor)]),
                      selected = "_")
    updateSelectInput(session, "pDataForAdditionalLabelling",
                      choices = colnames(pDataSubs()))
  })
  
  observeEvent(input$exportPlot, {
    showModal(modalDialog(
      title = "Export Settings",
      numericInput("modal_width", "Width (inches)", value = 7, min = 1),
      numericInput("modal_height", "Height (inches)", value = 5, min = 1),
      downloadButton("confirmDownload", "Download PDF"),
      easyClose = TRUE
    ))
  })
  
  output$confirmDownload <- downloadHandler(
    filename = function() { "Multi Dimensional Scaling.pdf" },
    
    content = function(file) {
      width <- input$modal_width
      height <- input$modal_height
      
      validate(
        need(!is.null(width) && !is.null(height), "Width and height must be specified.")
      )
      
      ggplot2::ggsave(file, plot = p()$plt, device = "pdf", width = width, height = height)
    }
  )

}

shinyApp(ui = ui, server = server)

# output$exportPlot <- downloadHandler(
#   filename = function() { "MDSplot.pdf" },
#   content = function(file) {
#     ggplot2::ggsave(file, plot = p()$plt, device = "pdf")
#   }
# )