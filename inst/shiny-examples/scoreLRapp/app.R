library(shiny)
require(ggplot2)
require(dplyr)

## Shiny app will go here



# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("ScoreLR App"),
  selectInput("select", label = h3("Select Method"), 
              choices = list("Ignore Dependence" = 1, "Average Features" = 2, "Independent Set" = 3, "Multiple KDE"=4), 
              selected = 1),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
  ,
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("hist")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$hist <- renderPlot({
    if(input$distribution=="normal") {
      xmin <- -10
      dist <- rnorm(n = input$num, mean = input$mean, sd = input$sd)
    }
    if(input$distribution=="gamma") {
      xmin <- 0
      # need to get shape and scale
      dist <- rgamma(n = input$num, shape=input$shape, scale=input$scale)
    }
    data.frame(dist) %>%
      ggplot(aes(x = dist)) +
      geom_histogram(binwidth = 0.25) +
      xlim(c(xmin,10))
  })
}

# Bind ui and server together
shinyApp(ui, server)
