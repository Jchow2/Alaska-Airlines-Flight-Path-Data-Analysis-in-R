
#############################
# Airline Flight Path Demand
#############################
# Author: Justin Chow

# 1. Setup and Initialization ---------------------------------
# Clear the workspace
rm(list = ls())

# Load required libraries
library(readxl)     # For reading Excel files
library(dplyr)      # For data manipulation
library(zoo)        # For working with date objects
library(ggplot2)    # For data visualization
library(psych)      # For summary statistics
library(testthat)

# Set the working directory
# Replace this with your GitHub repository's local directory path
setwd("C:/Users/jsjch/OneDrive/Documents/github/airline-flight-demand-all/flight data")

# 2. Load Major Airport Datasets -----------------------------
# Load major airport datasets
load_airport_data <- function(file_name, airport_code) {
  data <- read_excel(file_name, col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
  data$origin <- airport_code
  return(data)
}

# Define file paths for major airports
airport_files <- list(
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

# Load and combine all major airport datasets
major_airports <- do.call(
  rbind,
  lapply(names(airport_files), function(airport_code) {
    load_airport_data(airport_files[[airport_code]], airport_code)
  })
)

# 3. Load Additional Datasets ---------------------------------
# Load additional datasets
flights <- read_excel("FLIGHTS.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
passengers <- read_excel("PASSENGERS.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
passengers_all <- read_excel("PASSENGERS_ALL.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)

# 4. Data Cleaning and Transformation -------------------------
# Filter out rows where "Month" is not numeric
major_airports <- major_airports %>%
  filter(!is.na(Month) & Month != "TOTAL") %>%
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month),
    date = as.yearmon(paste(Year, Month), "%Y %m")
  ) %>%
  arrange(date)

# 5. Summary Statistics ---------------------------------------
# Summary statistics for major airports
cat("Summary Statistics:\n")
print(summary(major_airports))
cat("\nDescription:\n")
print(describe(major_airports))

# Save the cleaned dataset for future use
write.csv(major_airports, "cleaned_major_airports.csv", row.names = FALSE)
