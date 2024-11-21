library(shiny)
library(ggplot2)
library(CytoMDS)

mdsObj <- readRDS(file = "/Users/andreavicini/Desktop/InputObjects/HBV_mdsObj.rds")
pData <- readRDS(file = "/Users/andreavicini/Desktop/InputObjects/HBV_phenoData.rds")
stats <- readRDS(file = "/Users/andreavicini/Desktop/InputObjects/HBV_stats.rds")

ui <- fluidPage(
  titlePanel("Plot of Metric MDS object"),
  tabsetPanel(
    id = "tabs", # Tab ID
    # View Tab
    tabPanel("View",
             sidebarLayout(
               sidebarPanel(
                 selectInput("axis1", "X-axis coordinate:", choices = seq_len(nDim(mdsObj)), selected = 1),
                 selectInput("axis2", "Y-axis coordinate:", choices = seq_len(nDim(mdsObj)), selected = 2),
                 selectInput("colourBy", "Colour by:", choices = colnames(pData), selected = colnames(pData)[1]),
                 selectInput("labelBy", "Label by:", choices = colnames(pData), selected = colnames(pData)[2]),
                 selectInput("shapeBy", "Shape by:", choices = colnames(pData), selected = colnames(pData)[2]),
                 checkboxInput("biplot", "biplot", value = FALSE),
                 numericInput("arrowThreshold", "arrowThreshold", value = 0.8 , step = 0.1),
                 checkboxInput("flipXAxis", "Flip X-axis", value = FALSE),
                 checkboxInput("flipYAxis", "Flip Y-axis", value = FALSE)
               ),
               mainPanel(
                 plotOutput("mdsPlot", height = "500px")
               )
             )
    ),
    # General Settings Tab
    tabPanel("General Settings",
             sidebarLayout(
               sidebarPanel(
                 numericInput("pointLabelSize", "Point Label Size",
                              value = 3.88, step = 0.1),
                 checkboxInput("displayPointLabels", "displayPointLabels", value = TRUE),
                 checkboxInput("repelPointLabels", "repelPointLabels", value = TRUE),
                 checkboxInput("displayPseudoRSq", "Display pseudo RSquare", value = TRUE),
                 numericInput("pointSize", "Point size", value = 1 , step = 0.1),
                 checkboxInput("pointSizeReflectingStress", "pointSizeReflectingStress", value = FALSE),
                 checkboxInput("displayArrowLabels", "displayArrowLabels", value = TRUE),
                 numericInput("arrowLabelSize", "arrowLabelSize", value = 3.88, step = 0.1),
                 checkboxInput("repelArrowLabels", "repelArrowLabels", value = FALSE)
               ),
               mainPanel(
                 p("")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  output$mdsPlot <- renderPlot({
    projectionAxes <- c(as.integer(input$axis1), as.integer(input$axis2))
    CytoMDS::ggplotSampleMDS(
      mdsObj = mdsObj,
      pData = pData,
      projectionAxes = projectionAxes,
      biplot = input$biplot,
      extVariables = stats$median, # how to choose variable?
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
  })
}

# Run the application
shinyApp(ui = ui, server = server)
