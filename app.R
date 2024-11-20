library(shiny)
library(ggplot2)
library(CytoMDS)

mdsObj <- readRDS(file = "/Users/andreavicini/Desktop/InputObjects/HBV_mdsObj.rds")
pData <- readRDS(file = "/Users/andreavicini/Desktop/InputObjects/HBV_phenoData.rds")
stats <- readRDS(file = "/Users/andreavicini/Desktop/InputObjects/HBV_stats.rds")

ui <- fluidPage(
  titlePanel("Plot of Metric MDS object"),
  sidebarLayout(
    sidebarPanel(
      selectInput("axis1", "X-axis coordinate:", choices = seq_len(nDim(mdsObj)), selected = 1),
      selectInput("axis2", "Y-axis coordinate:", choices = seq_len(nDim(mdsObj)), selected = 2),
      selectInput("colourBy", "Colour by:", choices = colnames(pData), selected = colnames(pData)[1]),
      selectInput("labelBy", "Label by:", choices = colnames(pData), selected = colnames(pData)[2])
    ),
    mainPanel(
      plotOutput("mdsPlot")
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
      pDataForColour = input$colourBy,
      pDataForLabel = input$labelBy
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
