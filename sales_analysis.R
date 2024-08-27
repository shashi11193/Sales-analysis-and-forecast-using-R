# Load necessary libraries
library(shiny)
library(ggplot2)
library(forecast)
library(dplyr)

# Simulate sales data
set.seed(123)
dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
sales <- round(runif(24, 100, 1000))
sales_data <- data.frame(Date = dates, Sales = sales)

# ARIMA model fitting function
fit_arima <- function(data) {
  ts_data <- ts(data, frequency = 12)
  fit <- auto.arima(ts_data)
  return(fit)
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Retail Sales Dashboard"),
  sidebarLayout(
    sidebarPanel(
      helpText("Interactive visualization of sales data."),
      selectInput("forecastHorizon", "Forecast Horizon:",
                  choices = c("6 Months" = 6, "12 Months" = 12, "18 Months" = 18)),
      actionButton("forecastButton", "Generate Forecast")
    ),
    mainPanel(
      plotOutput("salesPlot"),
      plotOutput("forecastPlot")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  output$salesPlot <- renderPlot({
    ggplot(sales_data, aes(x = Date, y = Sales)) + 
      geom_line() + 
      labs(title = "Historical Sales Data", x = "Date", y = "Sales")
  })
  
  observeEvent(input$forecastButton, {
    horizon <- as.numeric(input$forecastHorizon)
    fit <- fit_arima(sales_data$Sales)
    future_sales <- forecast(fit, h = horizon)
    
    output$forecastPlot <- renderPlot({
      plot(future_sales)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
