library(shiny)
library(shinythemes)
library(ggplot2)
library(forecast)
library(dplyr)
library(readxl)
library(zoo)
library(plotly)

# Define UI for application
library(shiny)
library(shinythemes)  # For modern Shiny themes

ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Apply a Shiny theme
  titlePanel("Airline Passenger Demand Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Upload and Configure"),
      fileInput("datafile", "Upload Dataset", accept = c(".csv", ".xlsx")),
      h4("Model Selection"),
      selectInput("model", "Select Forecasting Model", choices = c("ETS", "ARIMA")),
      h4("Passenger Inputs"),
      numericInput("domestic", "Domestic Passengers", value = 100000, min = 0),
      numericInput("international", "International Passengers", value = 50000, min = 0),
      htmlOutput("total_input"),  # Styled total input dynamically updated
      actionButton("predict", "Predict Total Passengers", class = "btn btn-success")  # Styled button
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Overview",
                 h3("Uploaded Dataset Summary"),
                 tableOutput("data_summary"),
                 h3("Historical Passenger Trends"),
                 plotOutput("data_plot")),
        tabPanel("Forecasting",
                 h3("Forecasting Results"),
                 plotOutput("forecast_plot")),
        tabPanel("Prediction",
                 h3("Prediction Breakdown"),
                 htmlOutput("prediction_result"),  # Styled prediction breakdown
                 p("This prediction uses the historical trend to forecast the baseline number of passengers for the next month. ",
                   "It then combines this baseline forecast with your input for domestic and international passengers. ",
                   "The total input for passengers is calculated as the sum of your domestic and international inputs.")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive dataset upload and cleaning
  dataset <- reactive({
    req(input$datafile)
    data <- if (grepl("\\.csv$", input$datafile$name)) {
      read.csv(input$datafile$datapath)
    } else if (grepl("\\.xlsx$", input$datafile$name)) {
      read_excel(input$datafile$datapath)
    } else {
      stop("Invalid file format. Please upload a CSV or Excel file.")
    }
    
    data <- data %>%
      mutate(origin = ifelse(is.na(origin) | origin == "", "Unknown", origin)) %>%
      filter(grepl("^(0?[1-9]|1[0-2])$", Month)) %>%
      mutate(Month = as.numeric(Month)) %>%
      mutate(
        International = ifelse(is.na(International), 0, International),
        Total = ifelse(is.na(Total) | Total < 0, 0, Total)
      ) %>%
      filter(!is.na(Year), !is.na(Month), !is.na(Total))
    
    return(data)
  })
  
  # Display total of domestic and international passengers
  output$total_input <- renderText({
    total <- input$domestic + input$international
    paste("Total User Input (Domestic + International):", total)
  })
  
  # Historical data plot
  output$data_plot <- renderPlot({
    req(dataset())
    data <- dataset() %>%
      mutate(Date = as.yearmon(paste(Year, Month), "%Y %m")) %>%
      group_by(Date) %>%
      summarize(Total = sum(Total, na.rm = TRUE))
    
    ggplot(data, aes(x = Date, y = Total)) +
      geom_line(color = "blue") +
      labs(title = "Monthly Total Passenger Data", x = "Date", y = "Total Passengers") +
      theme_minimal()
  })
  
  # Improved forecasting plot
  output$forecast_plot <- renderPlot({
    req(dataset())
    
    # Aggregate data by Date
    data <- dataset() %>%
      mutate(Date = as.yearmon(paste(Year, Month), "%Y %m")) %>%
      group_by(Date) %>%
      summarize(Total = sum(Total, na.rm = TRUE))
    
    # Log transformation for stabilization
    data <- data %>% mutate(LogTotal = log1p(Total))  # log1p applies log(1 + x)
    
    ts_data <- ts(data$LogTotal, start = c(as.numeric(format(min(data$Date), "%Y")), 1), frequency = 12)
    
    # Apply selected model
    if (input$model == "ETS") {
      forecast_model <- ets(ts_data, model = "AAA")  # Additive error, trend, and seasonality
    } else if (input$model == "ARIMA") {
      forecast_model <- auto.arima(ts_data)
    }
    
    # Generate forecast (extended to 2021)
    forecast_result <- forecast(forecast_model, h = 24)  # Forecast for 2 years
    
    # Back-transform forecast to original scale
    forecast_result$mean <- expm1(forecast_result$mean)
    forecast_result$lower <- expm1(forecast_result$lower)
    forecast_result$upper <- expm1(forecast_result$upper)
    
    # Set forecast start date to January 2020
    forecast_start_date <- as.yearmon("2020-01")
    
    # Prepare forecast data frame
    forecast_df <- data.frame(
      Date = seq(from = forecast_start_date, by = 1/12, length.out = length(forecast_result$mean)),
      Total = forecast_result$mean,
      Lower = forecast_result$lower[, 2],  # 95% confidence lower bound
      Upper = forecast_result$upper[, 2]   # 95% confidence upper bound
    )
    
    # Create ggplot object
    ggplot() +
      geom_line(data = forecast_df, aes(x = Date, y = Total, color = "Forecast"), size = 1, linetype = "dashed") +
      geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
      scale_color_manual(values = c("Forecast" = "blue")) +
      labs(title = paste(input$model, "Forecast (2020-2021)"),
           x = "Date",
           y = "Total Passengers",
           color = "Legend") +
      theme_minimal()
  })
  
  # Prediction result with total input explanation
  output$prediction_result <- renderText({
    req(input$predict)
    
    # Aggregate data by Date
    data <- dataset() %>%
      mutate(Date = as.yearmon(paste(Year, Month), "%Y %m")) %>%
      group_by(Date) %>%
      summarize(Total = sum(Total, na.rm = TRUE))
    
    # Create a time series object
    ts_data <- ts(data$Total, start = c(as.numeric(format(min(data$Date), "%Y")), 1), frequency = 12)
    
    # Fit a forecasting model (ARIMA)
    forecast_model <- auto.arima(ts_data)
    
    # Forecast the baseline total passengers for the next time step
    forecast_result <- forecast(forecast_model, h = 1)  # Predict for next time step
    baseline_forecast <- forecast_result$mean[1]
    
    # Add user input to baseline forecast
    domestic <- input$domestic
    international <- input$international
    total_input <- domestic + international
    predicted_total <- baseline_forecast + total_input
    
    # Explanation for users
    HTML(paste0(
      "<h4><b>Prediction Breakdown:</b></h4>",
      "<ul>",
      "<li><span style='color:blue'><b>Baseline forecast from historical data:</b></span> ", round(baseline_forecast), " passengers</li>",
      "<li><span style='color:green'><b>User input for domestic passengers:</b></span> ", domestic, "</li>",
      "<li><span style='color:green'><b>User input for international passengers:</b></span> ", international, "</li>",
      "<li><span style='color:orange'><b>Total user input (domestic + international):</b></span> ", total_input, "</li>",
      "<li><span style='color:red'><b>Final prediction:</b></span> ", round(predicted_total), " total passengers</li>",
      "</ul>"
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

