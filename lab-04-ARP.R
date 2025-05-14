setwd("~/Downloads/ARPlab")

library(shiny)
library(mlbench)
library(ggplot2)
library(dplyr)

# Dataset Breast Cancer

data("BreastCancer")  # built-in dataset
bc <- BreastCancer[, -1]  # remove ID column
bc[ , 1:9] <- lapply(bc[ , 1:9], function(x) as.numeric(as.character(x)))  # convert to numeric
bc <- na.omit(bc)  # remove rows with NAs

ui <- fluidPage(
  titlePanel("Breast Cancer Dataset Viewer"),
  #Sidebar contain at least two interactive control
  sidebarLayout(
    sidebarPanel(
      selectInput("feature", "Select Feature:",
                  choices = names(bc)[1:9],
                  selected = "Cl.thickness"),

      sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 20),
      #At least one HTML
      HTML("<p><strong>Note:</strong> 'malignant' and 'benign' indicate diagnosis classes.</p>")
    ),

    mainPanel(
      plotOutput("histPlot")
    )
  )
)
## Main panel include one plot

server <- function(input, output) {
  #Include one reactive expression
  selected_data <- reactive({
    bc %>% select(Class, all_of(input$feature))
  })

  output$histPlot <- renderPlot({
    df <- selected_data() #reactive expression here
    ggplot(df, aes_string(x = input$feature, fill = "Class")) +
      geom_histogram(bins = input$bins, alpha = 0.6, position = "identity") +
      labs(title = paste("Distribution of", input$feature, "by Class"),
           x = input$feature, y = "Count") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
rsconnect::setAccountInfo(name='kunturs',
                          token='1B99472F6F4C79BEC32321CA547B17F9',
                          secret='raTtfGZ42nkf/QCPX2ZHxLPPbKA9RZu6gggJc9X9')

rsconnect::deployApp()


