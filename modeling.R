library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(broom)
library(gridExtra)
library(MASS)

leases <- read.csv("Leases.csv")
market_data <- read.csv("Price and Availability Data.csv")
unemployment <- read.csv("Unemployment.csv")
econ_indicators <- read.csv("economic_indicators_2018_2024.csv")
occupancy_data <- read.csv("Major Market Occupancy Data-revised.csv")


leases$date <- ymd(paste(leases$year, leases$monthsigned, "01", sep="-"))
if("date" %in% colnames(econ_indicators)) {
  econ_indicators$date <- as.Date(econ_indicators$date)
} else if("DATE" %in% colnames(econ_indicators)) {
  econ_indicators$date <- as.Date(econ_indicators$DATE)
} else {
  date_col <- colnames(econ_indicators)[1]
  econ_indicators$date <- as.Date(econ_indicators[[date_col]], format="%Y-%m-%d")
}

econ_indicators$year <- year(econ_indicators$date)
econ_indicators$quarter <- paste0("Q", quarter(econ_indicators$date))


quarterly_leases <- leases %>%
  group_by(year, quarter) %>%
  summarize(
    total_leased_sf = sum(leasedSF, na.rm = TRUE),
    avg_lease_size = mean(leasedSF, na.rm = TRUE),
    lease_count = n(),
    .groups = 'drop'
  )

quarterly_market <- market_data %>%
  group_by(year, quarter) %>%
  summarize(
    avg_availability = mean(availability_proportion, na.rm = TRUE),
    avg_rent = mean(overall_rent, na.rm = TRUE),
    total_available_space = sum(available_space, na.rm = TRUE),
    .groups = 'drop'
  )

quarterly_unemployment <- unemployment %>%
  group_by(year, quarter) %>%
  summarize(
    avg_unemployment = mean(unemployment_rate, na.rm = TRUE),
    .groups = 'drop'
  )

quarterly_econ <- econ_indicators %>%
  group_by(quarter, year) %>%
  summarize(
    avg_sp500 = mean(`S.P.500`, na.rm = TRUE),
    avg_treasury = mean(`X10Y.Treasury.Rate`, na.rm = TRUE),
    avg_fedfunds = mean(`Fed.Funds.Rate`, na.rm = TRUE),
    avg_cpi = mean(`Consumer.Price.Index..CPI.`, na.rm = TRUE),
    .groups = 'drop'
  )

quarterly_combined <- quarterly_leases %>%
  left_join(quarterly_market, by = c("year", "quarter")) %>%
  left_join(quarterly_unemployment, by = c("year", "quarter")) %>%
  left_join(quarterly_econ, by = c("year", "quarter"))

model <- lm(total_leased_sf ~ avg_sp500 + avg_treasury + avg_fedfunds + avg_cpi + avg_unemployment, 
            data = quarterly_combined)

model <- lm(total_leased_sf ~ avg_sp500 + avg_treasury + avg_fedfunds + avg_cpi + avg_unemployment +
              avg_sp500:avg_treasury + avg_sp500:avg_fedfunds + avg_sp500:avg_cpi + avg_sp500:avg_unemployment +
              avg_treasury:avg_fedfunds + avg_treasury:avg_cpi + avg_treasury:avg_unemployment +
              avg_fedfunds:avg_cpi + avg_fedfunds:avg_unemployment +
              avg_cpi:avg_unemployment,
            data = quarterly_combined)


final_model = step(model, direction="both", trace = F, k = log(nrow(quarterly_combined)))

summary(model)
