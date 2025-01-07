library(shiny)
library(ggplot2)

data_before <- read.csv("https://raw.githubusercontent.com/Missing-almost-everywhere/Missing-almost-everywhere.io/main/Projects/house-prices-advanced-regression-techniqueshouse-prices-advanced-regression-techniques-data/data_before_clean.csv")

ui <- fluidPage(
  titlePanel("House Prices Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:", choices = names(data_before))
    ),
    mainPanel(
      plotOutput("dynamicPlot")
    )
  )
)

server <- function(input, output) {
  output$dynamicPlot <- renderPlot({
    col <- input$variable
    
    if (is.numeric(data_before[[col]])) {
      ggplot(data_before, aes(x = .data[[col]])) + 
        geom_histogram(bins = 30, fill = 'skyblue', color = 'black', alpha = 0.7) +
        labs(title = paste('Histogram of', col), x = col, y = 'Frequency') +
        theme_minimal()
    } else {
      ggplot(data_before, aes(x = .data[[col]])) + 
        geom_bar(fill = 'orange', color = 'black', alpha = 0.7) +
        labs(title = paste('Bar Plot of', col), x = col, y = 'Count') +
        theme_minimal()
    }
  })
}

shinyApp(ui = ui, server = server)
