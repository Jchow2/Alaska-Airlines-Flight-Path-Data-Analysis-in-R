
###############################
# Airline Passenger Demand Analysis
###############################
# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(zoo)

# 1. Load and Combine Datasets ----------------------------
# Define file paths for major airports
file_paths <- list(
  DEN = "DEN.xls.xlsx",
  BWI = "BWI.xls.xlsx",
  DTW = "DTW.xls.xlsx",
  SLC = "SLC.xls.xlsx",
  PDX = "PDX.xls.xlsx",
  PHL = "PHL.xls.xlsx",
  MCO = "MCO.xls.xlsx",
  MSP = "MSP.xls.xlsx",
  IAH = "IAH.xls.xlsx",
  HNL = "HNL.xls.xlsx"
)

# Function to load and clean each dataset
load_airport_data <- function(file_path, airport_code) {
  df <- read_excel(file_path, skip = 2, col_names = c("Year", "Month", "Domestic", "International", "Total"))
  df$origin <- airport_code
  return(df)
}

# Load and combine airport datasets
airports <- lapply(names(file_paths), function(code) load_airport_data(file_paths[[code]], code))
major_airports <- bind_rows(airports)

# Load additional datasets
flights <- read_excel("FLIGHTS.xls.xlsx", skip = 2, col_names = c("Year", "Month", "Domestic", "International", "Total"))
passengers <- read_excel("PASSENGERS.xls.xlsx", skip = 2, col_names = c("Year", "Month", "Domestic", "International", "Total"))
passengers_all <- read_excel("PASSENGERS_ALL.xls.xlsx", skip = 2, col_names = c("Year", "Month", "Domestic", "International", "Total"))

# Combine all datasets into one
all_data <- bind_rows(major_airports, flights, passengers, passengers_all)

# 2. Data Cleaning ----------------------------------------
# Filter out rows where 'Month' is invalid
all_data <- all_data %>% filter(!is.na(as.numeric(Month)))
all_data$Month <- as.numeric(all_data$Month)

# Create a date column
all_data <- all_data %>%
  mutate(date = as.yearmon(paste(Year, Month), "%Y %m")) %>%
  arrange(date)

# Drop rows with missing Total
all_data <- all_data %>% filter(!is.na(Total))

# 3. Linear Regression Modeling --------------------------
# Split data into training and testing sets
train_data <- all_data %>% filter(Year < 2020)
test_data <- all_data %>% filter(Year >= 2020)

# Fit the linear regression model
lr_model <- lm(Total ~ Domestic + International, data = train_data)

# Predict on test data
test_data$predicted_total <- predict(lr_model, newdata = test_data)

# Evaluate model performance
mae <- mean(abs(test_data$Total - test_data$predicted_total))
mse <- mean((test_data$Total - test_data$predicted_total)^2)
cat("Linear Regression MAE:", mae, "\n")
cat("Linear Regression MSE:", mse, "\n")

# 4. Time-Series Forecasting -----------------------------
# Aggregate data by date
time_series <- all_data %>%
  group_by(date) %>%
  summarize(Total = sum(Total, na.rm = TRUE)) %>%
  arrange(date)

# Convert to time-series object
ts_total <- ts(time_series$Total, start = c(min(as.numeric(format(time_series$date, "%Y"))), 1), frequency = 12)

# ETS Model
ets_model <- ets(ts_total)
ets_forecast <- forecast(ets_model, h = 12)

# ARIMA Model
arima_model <- auto.arima(ts_total)
arima_forecast <- forecast(arima_model, h = 12)

# 5. Visualize Results ------------------------------------
# Plot regression predictions
ggplot(test_data, aes(x = Total, y = predicted_total)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Total Passengers",
       x = "Actual Total Passengers",
       y = "Predicted Total Passengers")

# Plot forecasts
autoplot(ts_total) +
  autolayer(ets_forecast$mean, series = "ETS Forecast", PI = FALSE, color = "blue") +
  autolayer(arima_forecast$mean, series = "ARIMA Forecast", PI = FALSE, color = "red") +
  labs(title = "Passenger Demand Forecast",
       x = "Date",
       y = "Total Passengers") +
  theme_minimal()

# Export your combined and cleaned dataset from R as a CSV file
write.csv(major_airports, "data_to_upload.csv", row.names = FALSE)