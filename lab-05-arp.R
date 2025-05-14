# https://kunturs.shinyapps.io/eustockmarkets-project5/
library(shiny)
library(tidyverse)
install.packages("ggthemes")
library(ggthemes)
library(plotly)

# Load and prepare the dataset
data(EuStockMarkets)
df <- as.data.frame(EuStockMarkets)
df$Time <- time(EuStockMarkets)

df_long <- pivot_longer(df, cols = -Time, names_to = "Index", values_to = "Price")

# UI
ui <- fluidPage(
  titlePanel("EU Stock Markets Dashboard"),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("indexSelect", "Select Stock Indices:",
                         choices = unique(df_long$Index),
                         selected = unique(df_long$Index)),

      sliderInput("yearRange", "Year Range (approx.):",
                  min = 1991, max = 1998, value = c(1991, 1998), step = 0.1)
    ),

    mainPanel(
      plotlyOutput("linePlot"),   # interactive plot
      plotOutput("histPlot")      # static histogram
    )
  )
)

# Server
server <- function(input, output) {

  # Reactive data
  filteredData <- reactive({
    df_long %>%
      filter(Index %in% input$indexSelect,
             Time >= input$yearRange[1],
             Time <= input$yearRange[2])
  })

  # Interactive line plot using plotly + ggplot2
  output$linePlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Time, y = Price, color = Index)) +
      geom_line() +
      theme_economist() +
      labs(title = "Stock Prices Over Time",
           x = "Year", y = "Closing Price")
    ggplotly(p)
  })

  # Static histogram
  output$histPlot <- renderPlot({
    ggplot(filteredData(), aes(x = Price, fill = Index)) +
      geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
      theme_light() +
      labs(title = "Distribution of Prices", x = "Price", y = "Count")
  })
}

# Launch the app
shinyApp(ui = ui, server = server)
shinyApp(ui = ui, server = server)
rsconnect::setAccountInfo(name='kunturs',
                          token='1B99472F6F4C79BEC32321CA547B17F9',
                          secret='raTtfGZ42nkf/QCPX2ZHxLPPbKA9RZu6gggJc9X9')

rsconnect::deployApp()
