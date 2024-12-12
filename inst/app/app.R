# mdsObj <- readRDS(system.file("app/data/HBV_mdsObj.rds", package = "MDSgui"))
# pData <- readRDS(system.file("app/data/HBV_phenoData.rds", package = "MDSgui"))
# stats <- readRDS(system.file("app/data/HBV_stats.rds", package = "MDSgui"))

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Plot of Metric MDS object"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Input files",
             sidebarLayout(
               sidebarPanel(
                 fileInput("mdsObjFile", "Choose MDS object file", accept = ".rds"),
                 fileInput("pDataFile", "Choose phenodataData file", accept = ".rds"),
                 fileInput("statsFile", "Choose stats file", accept = ".rds")
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

# server <- function(input, output, session) {
#   output$mdsPlot <- renderPlot({
#     CytoMDS::ggplotSampleMDS(
#       mdsObj = mdsObj,
#       pData = pData,
#       projectionAxes = c(as.integer(input$axis1), as.integer(input$axis2)),
#       biplot = input$biplot,
#       extVariables = stats$median, # how to choose variable?
#       pDataForColour = input$colourBy,
#       pDataForLabel = input$labelBy,
#       pDataForShape = input$shapeBy,
#       flipXAxis = input$flipXAxis,
#       flipYAxis = input$flipYAxis,
#       pointLabelSize = input$pointLabelSize,
#       displayPointLabels = input$displayPointLabels,
#       repelPointLabels = input$repelPointLabels,
#       displayPseudoRSq = input$displayPseudoRSq,
#       pointSizeReflectingStress = input$pointSizeReflectingStress,
#       pointSize = input$pointSize,
#       displayArrowLabels = input$displayArrowLabels,
#       arrowLabelSize = input$arrowLabelSize,
#       repelArrowLabels = input$repelArrowLabels,
#       arrowThreshold = input$arrowThreshold,
#       pDataForAdditionalLabelling = c("group", "patientId")
#     )
#   })
# }

server <- function(input, output, session) {
  mdsObj <- reactiveVal(NULL)
  pData <- reactiveVal(NULL)
  stats <- reactiveVal(NULL)

  observeEvent(input$mdsObjFile, {
    req(input$mdsObjFile)
    mdsObj(readRDS(input$mdsObjFile$datapath))
    updateSelectInput(session, "axis1", choices = seq_len(CytoMDS::nDim(mdsObj())), selected = 1)
    updateSelectInput(session, "axis2", choices = seq_len(CytoMDS::nDim(mdsObj())), selected = 2)
  })

  observeEvent(input$pDataFile, {
    req(input$pDataFile)
    pData(readRDS(input$pDataFile$datapath))
    updateSelectInput(session, "colourBy", choices = colnames(pData()), selected = colnames(pData())[1])
    updateSelectInput(session, "labelBy", choices = colnames(pData()), selected = colnames(pData())[2])
    updateSelectInput(session, "shapeBy", choices = colnames(pData()), selected = colnames(pData())[2])
  })

  observeEvent(input$statsFile, {
    req(input$statsFile)
    stats(readRDS(input$statsFile$datapath))
    updateSelectInput(session, "extVariables", choices = names(stats()), selected = names(stats())[1])
  })
  p <- reactive({
    if (length(input$pDataForAdditionalLabelling) == 0) {
      CytoMDS::ggplotSampleMDS(
        mdsObj = mdsObj(),
        pData = pData(),
        projectionAxes = c(as.integer(input$axis1), as.integer(input$axis2)),
        biplot = input$biplot,
        extVariables = stats()[[input$extVariables]],
        pDataForColour = input$colourBy,
        pDataForLabel = input$labelBy,
        pDataForShape = input$shapeBy,
        flipXAxis = input$flipXAxis,
        flipYAxis = input$flipYAxis,
        pointLabelSize = input$pointLabelSize,
        displayPointLabels = input$displayPointLabels,
        repelPointLabels = input$repelPointLabels,
        displayPseudoRSq = input$displayPseudoRSq,
        pointSizeReflectingStress = input$pointSizeReflectingStress,
        pointSize = input$pointSize,
        displayArrowLabels = input$displayArrowLabels,
        arrowLabelSize = input$arrowLabelSize,
        repelArrowLabels = input$repelArrowLabels,
        arrowThreshold = input$arrowThreshold
      )
    } else {
      CytoMDS::ggplotSampleMDS(
        mdsObj = mdsObj(),
        pData = pData(),
        projectionAxes = c(as.integer(input$axis1), as.integer(input$axis2)),
        biplot = input$biplot,
        extVariables = stats()[[input$extVariables]],
        pDataForColour = input$colourBy,
        pDataForLabel = input$labelBy,
        pDataForShape = input$shapeBy,
        flipXAxis = input$flipXAxis,
        flipYAxis = input$flipYAxis,
        pointLabelSize = input$pointLabelSize,
        displayPointLabels = input$displayPointLabels,
        repelPointLabels = input$repelPointLabels,
        displayPseudoRSq = input$displayPseudoRSq,
        pointSizeReflectingStress = input$pointSizeReflectingStress,
        pointSize = input$pointSize,
        displayArrowLabels = input$displayArrowLabels,
        arrowLabelSize = input$arrowLabelSize,
        repelArrowLabels = input$repelArrowLabels,
        arrowThreshold = input$arrowThreshold,
        pDataForAdditionalLabelling = input$pDataForAdditionalLabelling
      )
    }
    })
  output$mdsPlot_ <- renderUI({
    if (input$plotlytooltipping == FALSE) {
      plotOutput("mdsPlot")

    } else if (input$plotlytooltipping == TRUE){
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

  observeEvent(input$plotlytooltipping, {
    if (input$plotlytooltipping == TRUE) {
      shinyjs::disable("biplot")
    } else {
      shinyjs::enable("biplot")
    }
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
