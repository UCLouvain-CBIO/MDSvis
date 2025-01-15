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
                   condition = "input.biplot == false",
                   checkboxInput("plotlytooltipping", "Plotly", value = FALSE)
                 ),
                 conditionalPanel(
                   condition = "input.plotlytooltipping == true",
                   selectInput("pDataForAdditionalLabelling",
                               "pDataForAdditionalLabelling:",
                               choices = NULL, multiple = TRUE)
                 ),
                 conditionalPanel(
                   condition = "input.biplot == true",
                   selectInput("extVariables", "Select stat:", choices = NULL),
                   numericInput("arrowThreshold", "arrowThreshold", value = 0.8,
                                step = 0.1)
                 ),
                 checkboxInput("flipXAxis", "Flip X-axis", value = FALSE),
                 checkboxInput("flipYAxis", "Flip Y-axis", value = FALSE)
               ),
               mainPanel(
                 uiOutput("mdsPlot_")
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
      if (!is.null(input$statsFile)) {
        shinyjs::enable("displayArrowLabels")
        shinyjs::enable("arrowLabelSize")
        shinyjs::enable("repelArrowLabels")
      }
      if (!is.null(input$statsFile) && !input$plotlytooltipping) {
        shinyjs::enable("biplot")
      } else {
        shinyjs::disable("biplot")
      }
      if (input$biplot == FALSE) {
        shinyjs::enable("plotlytooltipping") #####???
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
    mdsObj(readRDS(input$mdsObjFile$datapath))
    updateSelectInput(session, "axis1",
                      choices = seq_len(CytoMDS::nDim(mdsObj())), selected = 1)
    updateSelectInput(session, "axis2",
                      choices = seq_len(CytoMDS::nDim(mdsObj())), selected = 2)
  })

  observeEvent(input$pDataFile, {
    req(input$pDataFile)
    pData(readRDS(input$pDataFile$datapath))
    pDataSubs(readRDS(input$pDataFile$datapath))
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

  observeEvent(input$statsFile, {
    req(input$statsFile)
    stats(readRDS(input$statsFile$datapath))
    updateSelectInput(session, "extVariables", choices = names(stats()),
                      selected = names(stats())[1])
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
      # do.call(CytoMDS::ggplotSampleMDS, plotargs)
    if (!is.null(input$pDataFile)) {#no biplot
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
    do.call(CytoMDS::ggplotSampleMDS, plotargs)
  })

  output$mdsPlot_ <- renderUI({
    if (input$plotlytooltipping == FALSE) {
      plotOutput("mdsPlot")

    } else if (input$plotlytooltipping == TRUE) {
      plotly::plotlyOutput("mdsPlotly")
    }
  })

  output$mdsPlot <- renderPlot({
    req(mdsObj())
    p()
  })

  output$mdsPlotly <- plotly::renderPlotly({
    req(mdsObj())
    plotly::ggplotly(p())
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
