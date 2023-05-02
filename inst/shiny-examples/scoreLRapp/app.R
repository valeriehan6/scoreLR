library(shiny)
require(ggplot2)
require(dplyr)

## Shiny app will go here

shoedata_split <- dep_split(shoedata, 0.8, 585)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("ScoreLR App"),
  # selectInput("select", label = h3("Select Method"), 
  #             choices = list("Ignore Dependence" = 1, "Average Features" = 2, "Independent Set" = 3, "Multiple KDE"=4), 
  #             selected = 1),
  selectizeInput(
    "select", "Select Method", choices = list("Ignore Dependence" = 1, "Average Features" = 2, 
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
    #textOutput("patience")
  ),
  plotOutput("slrplot")
)

server <- function(input, output) {
  
  #skipPlot <- reactiveVal(value = 1)
  
  # output$patience <- renderText({
  #   if(input$select == 1){
  #     result <- "Please be patient! Your ROC curve for the SLR fit via the Ignore Dependence method is coming soon!"
  #   }
  #   else if(input$select == 2){
  #     result <- "Please be patient! Your ROC curve for the SLR fit via the Average Features method is coming soon!"
  #   }
  #   else if(input$select == 3){
  #     result <- "Please be patient! Your ROC curve for the SLR fit via the Independent Set method is coming soon!"
  #   }
  #   else if(input$select == 4){
  #     result <- "Please be patient! Your ROC curve for the SLR fit via the Multiple KDE method is coming soon!"
  #   }
  # })
  
  output$slrplot <- renderPlot({
    # if (isolate(skipPlot()==1)) {
    #   # skip first reactive sequence
    #   skipPlot(0)
    #   # launch next reactive sequence
    #   invalidateLater(10, session = getDefaultReactiveDomain())
    # } else {
    if(input$select==1) {
      p <- plot_slr_roc(shoedata_split, "IgnoreDependence", num_runs = 10)
    }
    if(input$select==2) {
      p <- plot_slr_roc(shoedata_split, "AverageFeatures", num_runs = 10)
    }
    if(input$select==3) {
      p <- plot_slr_roc(shoedata_split, "StrictIndependentSet", num_runs = 10)
    }
    if(input$select==4) {
      p <- plot_slr_roc(shoedata_split, "MultipleKDE", num_runs = 10)
    }
    p
    #}
  })
}

# Bind ui and server together
shinyApp(ui, server)
