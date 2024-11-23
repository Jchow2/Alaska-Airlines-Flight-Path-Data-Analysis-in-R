# ✈️ Airline Flight Path Demand Analysis

A data-driven project exploring how the COVID-19 pandemic impacted airline flight path demand. Using R, this project examines historical flight data, visualizes trends, and builds predictive models to better understand the changing landscape of air travel.

---

## 📝 Table of Contents
- [Project Overview](#project-overview)
- [Features](#features)
- [Data Sources](#data-sources)
- [Installation](#installation)
- [Usage](#usage)
- [Results](#results)
- [License](#license)

---

## 📖 Project Overview
The COVID-19 pandemic drastically altered the demand for air travel, causing significant shifts in passenger behaviors and airline operations. This project focuses on:
- Analyzing flight path demand during the pandemic.
- Identifying trends in passenger travel across major routes.
- Predicting recovery patterns for the airline industry.

This analysis provides insights for airlines, policymakers, and travelers to adapt to the "new normal" in aviation.

---

## 🌟 Features
- **Exploratory Data Analysis (EDA):** Visualize trends in flight frequency and demand.
- **COVID-19 Impact Analysis:** Evaluate changes in demand by geographic region and airline.
- **Predictive Modeling:** Use regression and time series models to forecast future flight demand.
- **Interactive Visualizations:** Build plots and dashboards to summarize findings.

---

## 📊 Data Sources
This project utilizes the following datasets:
1. **[Bureau of Transportation Statistics (BTS)](https://www.transtats.bts.gov/):** Contains flight data and passenger counts.
2. **COVID-19 Data:** Case trends and restrictions by region.
3. **OpenSky Network:** Real-time flight tracking data.

---

## ⚙️ Installation
To run this project locally, follow these steps:

1. Clone this repository:
   ```bash
   git clone https://github.com/Jchow2/R-airline-covid19.git
   cd R-airline-covid19
## Data
Transtats - United States Bureau of Transportation Statistics: https://www.transtats.bts.gov/

## Install Required R Packages
```r
install.packages(c("tidyverse", "lubridate", "forecast", "ggplot2", "shiny"))
```
## Open the R project file and Load Script
 ```r
source("airline-covid.R")
```
## Usage

### 1. Exploratory Data Analysis

Perform an exploratory analysis of the dataset to generate summary statistics and visualizations.

### Run exploratory analysis script
```r
source("eda_airline_flight_demand.R")
```
This script outputs key metrics, trends, and visual summaries of the dataset, helping you understand passenger demand patterns.

### 2. Predict Future Demand

Estimate future trends in passenger demand using predictive models.

### Run predictive modeling script
```r
source("airline_predict_passenger_demand.R")
```
This script uses time-series forecasting models to predict passenger trends for upcoming months based on historical data.

### 3. Interactive Dashboard

Explore the results interactively through a Shiny application.

### Launch the Shiny dashboard
```r
shiny::runApp("shiny_app.R")
```
The dashboard provides a user-friendly interface to view predictions, analyze trends, and customize inputs for demand forecasting.

## Results
Key insights from the analysis include:

### International Demand Impact:

Significant declines in passenger demand on international routes during 2020, attributed to the pandemic's travel restrictions.
### Regional Domestic Recovery:

Domestic travel showed faster recovery rates in certain regions, likely influenced by localized policy changes and consumer behavior.
### Forecasting Insights:

Predictive models indicate a gradual recovery in passenger demand, with projections suggesting near-normal levels by 2023.
Regional variations highlight the importance of localized strategies for recovery.
### Interactive Visualization:

Explore detailed visualizations and insights through the Shiny app, providing a user-friendly dashboard for demand analysis.
Detailed results, visualizations, and outputs are available in the results directory.

## 📜 License

This project is licensed under the MIT License.

## 👩‍💻 Author

Developed and maintained by Justin Chow.
Feel free to connect on LinkedIn or reach out via email at jsjchow23@gmail.com.

