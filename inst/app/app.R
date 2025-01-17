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
               mainPanel(
                 p("")
               )
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
                 checkboxInput("biplot", "Biplot", value = FALSE),
                 conditionalPanel(
                   condition = "input.biplot == true",
                   selectInput("extVariables", "Select stat:", choices = NULL),
                   numericInput("arrowThreshold", "arrowThreshold", value = 0.8,
                                step = 0.1)
                 ),
                 checkboxInput("plotlytooltipping", "Plotly", value = FALSE),
                 conditionalPanel(
                   condition = "input.plotlytooltipping == true",
                   selectInput("pDataForAdditionalLabelling",
                               "pDataForAdditionalLabelling:",
                               choices = NULL, multiple = TRUE)
                 ),
                 checkboxInput("flipXAxis", "Flip X-axis", value = FALSE),
                 checkboxInput("flipYAxis", "Flip Y-axis", value = FALSE)
               ),
               mainPanel(
                 #uiOutput("mdsPlot_")
                 plotOutput("mdsPlot"),
                 plotly::plotlyOutput("mdsPlotly")
               )
             )
    ),
    tabPanel("General Settings",
             sidebarLayout(
               sidebarPanel(
                 numericInput("pointLabelSize", "Point Label Size",
                              value = 3.88, step = 0.1),
                 checkboxInput("displayPointLabels", "displayPointLabels",
                               value = TRUE),
                 conditionalPanel(
                   condition = "input.displayPointLabels == true",
                   numericInput("maxOverlaps", "max.overlaps", value = 10,
                                step = 1)
                 ),
                 checkboxInput("repelPointLabels", "repelPointLabels",
                               value = TRUE),
                 checkboxInput("displayPseudoRSq", "Display pseudo RSquare",
                               value = TRUE),
                 numericInput("pointSize", "Point size", value = 1, step = 0.1),
                 checkboxInput("pointSizeReflectingStress",
                               "pointSizeReflectingStress", value = FALSE),
                 checkboxInput("displayArrowLabels", "displayArrowLabels",
                               value = TRUE),
                 numericInput("arrowLabelSize", "arrowLabelSize", value = 3.88,
                              step = 0.1),
                 checkboxInput("repelArrowLabels", "repelArrowLabels",
                               value = FALSE)
               ),
               mainPanel(
                 p("")
               )
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

  observe({
    if (is.null(input$mdsObjFile)) {
      shinyjs::disable("axis1")
      shinyjs::disable("axis2")
      shinyjs::disable("colourBy")
      shinyjs::disable("labelBy")
      shinyjs::disable("shapeBy")
      shinyjs::disable("biplot")
      shinyjs::disable("plotlytooltipping") #########
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
    } else {
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
      if (!is.null(input$statsFile)) {
        shinyjs::enable("biplot")
        shinyjs::enable("displayArrowLabels")
        shinyjs::enable("arrowLabelSize")
        shinyjs::enable("repelArrowLabels")
      }
      if (!is.null(input$pDataFile)){
        shinyjs::enable("colourBy")
        shinyjs::enable("labelBy")
        shinyjs::enable("shapeBy")
      }
    }
  })

  observeEvent(input$mdsObjFile, {
    req(input$mdsObjFile)
    tryCatch({
      mds <- readRDS(input$mdsObjFile$datapath)

      if (is(mds)[1] != "MDS") {
        stop("The selected file does not contain a MDS object.")
      }

      mdsObj(mds)
      updateSelectInput(session, "axis1",
                        choices = seq_len(CytoMDS::nDim(mdsObj())), selected = 1)
      updateSelectInput(session, "axis2",
                        choices = seq_len(CytoMDS::nDim(mdsObj())), selected = 2)
    }, error = function(e) {
      showNotification(as.character(e$message), type = "error", duration = 5)
    })
  })

  observeEvent(input$pDataFile, {
    req(input$pDataFile)
    tryCatch({
      pdata <- readRDS(input$pDataFile$datapath)

      if (!is.data.frame(pdata)) {
        stop("The selected file does not contain a data.frame object.")
      }

      pData(pdata)
      pDataSubs(pdata)

      updateSelectInput(session, "colourBy",
                        choices = c("_", colnames(pDataSubs())[sapply(pDataSubs(), is.factor)]),
                        selected = "_")
      updateSelectInput(session, "labelBy",
                        choices = c("_", colnames(pDataSubs())),
                        selected = "_")
      updateSelectInput(session, "shapeBy",
                        choices = c("_", colnames(pDataSubs())[sapply(pDataSubs(), is.factor)]),
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
        stop("The selected file does not contain a list of matrices")
      }
      stats(stats_)
      updateSelectInput(session, "extVariables", choices = names(stats()),
                        selected = names(stats())[1])

    }, error = function(e) {
      showNotification(as.character(e$message), type = "error", duration = NULL)
    })
  })



  p <- reactive({
    plotargs = list(
      mdsObj = mdsObj(),
      projectionAxes = c(as.integer(input$axis1), as.integer(input$axis2)),
      flipXAxis = input$flipXAxis,
      flipYAxis = input$flipYAxis,
      pointLabelSize = input$pointLabelSize,
      displayPointLabels = input$displayPointLabels,
      repelPointLabels = input$repelPointLabels,
      displayPseudoRSq = input$displayPseudoRSq,
      pointSizeReflectingStress = input$pointSizeReflectingStress,
      pointSize = input$pointSize
    )
    if (!is.null(input$maxOverlaps)) {
      plotargs$max.overlaps = input$maxOverlaps
    }
    if (!is.null(input$statsFile)) {
      plotargs$biplot = input$biplot
      plotargs$extVariables = stats()[[input$extVariables]]
      plotargs$displayArrowLabels = input$displayArrowLabels
      plotargs$arrowLabelSize = input$arrowLabelSize
      plotargs$repelArrowLabels = input$repelArrowLabels
      plotargs$arrowThreshold = input$arrowThreshold
     }
    if (!is.null(input$pDataFile)) {
      plotargs$pData = pDataSubs()
      if (input$colourBy != "_") {
        plotargs$pDataForColour = input$colourBy
      }
      if (input$labelBy != "_") {
        plotargs$pDataForLabel = input$labelBy
      }
      if (input$shapeBy != "_") {
        plotargs$pDataForShape = input$shapeBy
      }
     }
    if (length(input$pDataForAdditionalLabelling)) {
      plotargs$pDataForAdditionalLabelling = input$pDataForAdditionalLabelling
    }
    list(plt = do.call(CytoMDS::ggplotSampleMDS, plotargs[!(names(plotargs) %in% "pDataForAdditionalLabelling")]),
         pltly = do.call(CytoMDS::ggplotSampleMDS, plotargs[!(names(plotargs) %in% "biplot")]))
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
                      choices = c("_", colnames(pDataSubs())[sapply(pDataSubs(), is.factor)]),
                      selected = "_")
    updateSelectInput(session, "labelBy",
                      choices = c("_", colnames(pDataSubs())),
                      selected = "_")
    updateSelectInput(session, "shapeBy",
                      choices = c("_", colnames(pDataSubs())[sapply(pDataSubs(), is.factor)]),
                      selected = "_")
  })

  observeEvent(input$pDataForAdditionalLabelling, {
    if (length(input$pDataForAdditionalLabelling) > 3) {
      updateSelectInput(session, "pDataForAdditionalLabelling",
                        selected = input$pDataForAdditionalLabelling[1:3])
      showNotification("You can select a maximum of 3 options for pDataForAdditionalLabelling.",
                       type = "warning")
    }
  })

}

shinyApp(ui = ui, server = server)
