library(shiny)
library(ggplot2)
library(dplyr)

data_final <- read.csv("https://raw.githubusercontent.com/Missing-almost-everywhere/Missing-almost-everywhere.io/main/Projects/house-prices-advanced-regression-techniqueshouse-prices-advanced-regression-techniques-data/data_for_final_model.csv")

ui <- fluidPage(
  titlePanel("House Price Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:", 
                  choices = names(data_final))
    ),
    mainPanel(
      plotOutput("dynamicPlot")
    )
  )
)

server <- function(input, output) {
  output$dynamicPlot <- renderPlot({
    col <- input$variable
    
    if (is.numeric(data_final[[col]])) {
      if(col != "SalePrice") {
        ggplot(data_final, aes(x = .data[[col]], y = log(SalePrice))) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", color = "red") +
          labs(title = paste(col, "vs log(SalePrice)")) +
          theme_minimal()
      } else {
        ggplot(data_final, aes(x = .data[[col]])) +
          geom_histogram(bins = 30, fill = 'skyblue', color = 'black', alpha = 0.7) +
          labs(title = paste('Histogram of', col)) +
          theme_minimal()
      }
    } else {
      ggplot(data_final, aes(x = .data[[col]], y = log(SalePrice))) +
        geom_violin(fill = 'skyblue', alpha = 0.7) +
        stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
        labs(title = paste(col, "vs log(SalePrice)")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

shinyApp(ui = ui, server = server)
