library(shiny)
library(shinybusy)
require(ggplot2)
require(dplyr)

## Shiny app will go here

shoedata_split <- dep_split(shoedata, 0.8, 585)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  add_busy_spinner(spin = "fingerprint"),
  
  # Application title
  titlePanel("ScoreLR App"),
  mainPanel("Please be patient. The plotting function takes a moment to run after you select the number of runs
            and a method."),
  numericInput("num_runs", label = h3("Number of Runs"), value = 1, min = 1, max = 200),
  
  numericInput("p", label = h3("Training/Testing Split Proportion"), value = 0.75, min = 0, max = 1),
  
  numericInput("alpha", label = h3("Line Transparency"), value = 0.1, min = 0, max = 1),
  
  selectizeInput("select", label = h3("Select Method"), choices = list("Ignore Dependence" = 1, "Average Features" = 2, 
                                              "Independent Set" = 3, "Multiple KDE" = 4),
    options = list(
      placeholder = 'Please select a method below',
      onInitialize = I('function() { this.setValue(""); }')
    )
  ),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  
  # Show a plot of the SLR
  mainPanel(
  ),
  plotOutput("slrplot")
)

server <- function(input, output) {
  
  output$slrplot <- renderPlot({

    if(input$select==1) {
      p <- plot_slr_roc(data = shoedata_split, p = input$p, method = "IgnoreDependence", 
                        num_runs = input$num_runs, alpha = input$alpha)
    }
    if(input$select==2) {
      p <- plot_slr_roc(data = shoedata_split, p = input$p, method = "AverageFeatures", 
                        num_runs = input$num_runs, alpha = input$alpha)
    }
    if(input$select==3) {
      p <- plot_slr_roc(data = shoedata_split, p = input$p, method = "StrictIndependentSet", 
                        num_runs = input$num_runs, alpha = input$alpha)
    }
    if(input$select==4) {
      p <- plot_slr_roc(data = shoedata_split, p = input$p, method = "MultipleKDE", 
                        num_runs = input$num_runs, alpha = input$alpha)
    }
    p
  })
}

# Bind ui and server together
shinyApp(ui, server)
