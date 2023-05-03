
#############################
# Airline Flight Path Demand#
#############################
# Justin Chow

# Clear the working space
rm(list = ls())

# Set working directory
setwd("C:/Users/14087/Documents/github/airline-covid19")

library(AER)
library(doBy)
library(dplyr)
library(ggplot2)
library(psych)
library(readxl)
library(stargazer)
library(tidyr)
library(zoo)

# Load 10 randomly assigned Major Airports for Alaska Airlines
den <- readxl::read_excel("DEN.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
bwi <- readxl::read_excel("BWI.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
dtw <- readxl::read_excel("DTW.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
slc <- readxl::read_excel("SLC.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
pdx <- readxl::read_excel("PDX.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
phl <- readxl::read_excel("PHL.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
mco <- readxl::read_excel("MCO.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
msp <- readxl::read_excel("MSP.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
iah <- readxl::read_excel("IAH.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
hnl <- readxl::read_excel("HNL.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)

# Rename assigned Major Airports data sets
den$origin <- "DEN"
bwi$origin <- "BWI"
dtw$origin <- "DTW"
slc$origin <- "SLC"
pdx$origin <- "PDX"
phl$origin <- "PHL"
mco$origin <- "MCO"
msp$origin <- "MSP"
iah$origin <- "IAH"
hnl$origin <- "HNL"

# Total number of Flights and Passengers for Alaska Airlines
flights <- readxl::read_excel("FLIGHTS.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
passengers <- readxl::read_excel("PASSENGERS.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)

# Bind 10 randomly assigned airports into one variable
major <- rbind(bwi,den,dtw,hnl,iah,mco,msp,pdx,phl,slc)

# Covert the variables in the major data set everything to lower case
names(major) <- tolower(names(major))
major <- major[major$month != "TOTAL", ]
major <- major[order(major$year,decreasing=FALSE),]

# Format year and month to create datetime variable 
major$date <- as.yearmon(paste(major$year, major$month), "%Y %m")
major <- major[order(major$date,decreasing=FALSE),]

# Return summary statistics and describe the data frame
summary(major)
describe(major)

# Time Series plot in domestic flights for all flights from all major airports
ggplot(major, aes(x=date, y=domestic, colour = origin)) + geom_line() +
  labs(title = "All Major Airline Flights", x = "Dates", y = "Flights") 

# Subset major airlines to omit the outlier flights from Phoenix airport
major_all <- subset(major, origin != 'PDX')

# Time Series plot in domestic flights for all flights from major airports except from Phoenix
ggplot(major_all, aes(x=date, y=domestic, colour = origin)) + geom_line() +
  labs(title = "All Major Airline Flights", x = "Dates", y = "Flights") 

# Subset flights from major airlines that occur from the beginning of 2019 to the end of 2020
subset1 <- subset(major_all, year >= 2019 & year <= 2020)

# Stacked bar chart in domestic flights for all flights from major airports except from Phoenix
ggplot(subset1, aes(x=date)) + 
  geom_bar(aes(y = domestic/sum(domestic), fill=origin), stat="identity") +
  labs(title = "All Major Flights 2019-2020", x = "Dates", y = "Percent Change") 

