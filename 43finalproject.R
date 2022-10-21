
########################
# ECON 43 Final Project#
########################
# Justin Chow
# Mar 15, 2020

# Clear the working space
rm(list = ls())

# Set working directory
setwd("~/SCU_Winter2021/Econ 43/Econ 43_2.11.21/mydata")

library(readxl)
library(dplyr)
library(psych)
library(stargazer)

# Load 10 randomly assigned Major Airports for Alaska Airlines
den <- readxl::read_excel("DEN.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
den$origin <- "DEN"
bwi <- readxl::read_excel("BWI.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
bwi$origin <- "BWI"
dtw <- readxl::read_excel("DTW.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
dtw$origin <- "DTW"
slc <- readxl::read_excel("SLC.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
slc$origin <- "SLC"
pdx <- readxl::read_excel("PDX.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
pdx$origin <- "PDX"
phl <- readxl::read_excel("PHL.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
phl$origin <- "PHL"
mco <- readxl::read_excel("MCO.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
mco$origin <- "MCO"
msp <- readxl::read_excel("MSP.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
msp$origin <- "MSP"
iah <- readxl::read_excel("IAH.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
iah$origin <- "IAH"
hnl <- readxl::read_excel("HNL.xls.xlsx", col_names = c("Year", "Month", "Domestic", "International", "Total"), skip = 2)
hnl$origin <- "HNL"

# Total number of Flights and Passengers for Alaska Airlines
flights <- readxl::read_excel("FLIGHTS.xls.xlsx", 
                              col_names = c("Year", "Month", "Domestic", "International", "Total"), 
                              skip = 2)
passengers <- readxl::read_excel("PASSENGERS.xls.xlsx", 
                                 col_names = c("Year", "Month", "Domestic", "International", "Total"), 
                                 skip = 2)

# Bind 10 randomly assigned airports into one variable
major <- rbind(bwi,den,dtw,hnl,iah,mco,msp,pdx,phl,slc)

# covert everything to lower case
names(major) <- tolower(names(major))
View(major)

# drop missing values in Domestic flights in the major airport dataset 
major <- major %>% filter(!is.na(domestic))

reg <- lm(year ~ month + domestic + international + total + origin, data=major)

summary(reg)
stargazer(reg,
          title="regressions", type="text",
          df=FALSE, digits=3)

### Descriptive Statistics
View(major)
mean(major$domestic, na.rm = TRUE)
median(major$domestic)

# measure of variability 
range(major$domestic)
quantile(major$domestic, probs = c(0.25, 0.75), na.rm = TRUE)
IQR(major$domestic, na.rm = TRUE)
skew(major$domestic)
kurtosi(major$domestic)

summary(major$domestic)
summary(major)

# Describe a data frame 
describe(major)
describeBy(major, group=major$domestic)

# Correlation
year <- as.numeric(major$year)
cor(major$domestic, year)
cor(major$domestic, year, method="spearman")

# Subset major airports and find number of flights before and after COVID-19 pandemic 
subset1 <- subset(major, year >= 2017 & origin == "DEN" & month == 3)
View(subset1)

# Create log flights for major dataset
major$lnflights <- log(major$domestic) 
# visualizing 
attach(major)
table(origin)
table(domestic)
hist(lnflights, breaks=4, main="Frequencies Alaska Airline flies to another major airports", 
     xlab="Major Airport", labels = TRUE)



### Regressions - Major Airports
# Read a regression of Year on Domestic flights to major airports 
reg1 <- lm(year ~ lnflights + month + origin + total, data = as.data.frame(major))

# Create a subset of major not including total number of flights
subset2 <- subset(major, month != "TOTAL")
reg2 <- lm(year ~ lnflights + month + origin + total, data = as.data.frame(subset2))

# Report the required regression results in a single stargazer table.
stargazer(reg1, reg2,
          title="Major Airport flights Regression Results", type="text", 
          df=FALSE, digits=3)

# Subset year on Domestic flights from all major airport except Alaska Flights headquarter from 2017 to 2020
install.packages("maps")
library(maps)

subset3 <- subset(major, year >= 2017 & origin != "PDX" & month != "TOTAL")
View(subset3)
max(subset3$domestic)
min(subset3$domestic)

# Read a regression of Year on Domestic flghts using subset above
reg3 <- lm(year ~ lnflights + month + origin + total, data=as.data.frame(subset3))

# Report the required regression results in a single stargazer table.
stargazer(reg1, reg2, reg3,
          title="Major Airport flights Regression Results", type="text", 
          df=FALSE, digits=3)


### Regressions - Alaska Airlines Flights
# Read a regression of Year on all domestic flights from Alaska Airlines
flights$lnflights <- log(flights$domestic)
reg4 <- lm(year ~ lnflights + month + international + total, data=as.data.frame(flights))

# Create a subset of Alaska Airlines flights for month not including total number of flights
subset3 <- subset(flights, month != "TOTAL")
reg5 <- lm(year ~ lnflights + month + international + total, data = as.data.frame(subset3))

# Report the required regression results in a single stargazer table.
stargazer(reg4, reg5,
          title="Alaska Airline flights Regression Results", type="text", 
          df=FALSE, digits=3)


### Regressions - Alaska Airlines Passengers
# Read a regression of Year on all domestic passengers from Alaska Airlines
passengers$lnflights <- log(passengers$domestic)
reg6 <- lm(year ~ lnflights + month + international + total, data=as.data.frame(passengers))

# Create a subset of Alaska Airlines passengers for month not including total number of flights
subset4 <- subset(passengers, month != "TOTAL")
reg7 <- lm(year ~ lnflights + month + international + total, data = as.data.frame(subset3))

# Report the required regression results in a single stargazer table.
stargazer(reg6, reg7,
          title="Alaska Airline flights Regression Results", type="text", 
          df=FALSE, digits=3)

