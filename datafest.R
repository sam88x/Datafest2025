library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(broom)
library(gridExtra)

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

model_summary <- summary(model)
model_tidy <- tidy(model)
model_glance <- glance(model)


ggplot(quarterly_combined) +
  geom_bar(aes(x = as.factor(paste(year, quarter)), y = total_leased_sf/1000000), 
           stat = "identity", fill = "darkblue", alpha = 0.7) +
  geom_line(aes(x = as.factor(paste(year, quarter)), y = avg_sp500 * 10000, group = 1), 
            color = "red", size = 1) +
  scale_y_continuous(
    name = "Total Leased Space (Million sq ft)",
    sec.axis = sec_axis(~./10000, name = "S&P 500 Index")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "S&P 500 Index vs. Commercial Leasing Volume",
    x = "Year-Quarter",
    caption = "Source: Leasing and Economic Data"
  )

quarterly_combined$predicted <- predict(model)

p2 <- ggplot(quarterly_combined, aes(x = total_leased_sf/1000000, y = predicted/1000000)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Actual vs. Predicted Leasing Volume",
    x = "Actual Leasing Volume (Million sq ft)",
    y = "Predicted Leasing Volume (Million sq ft)"
  )

model_tidy %>%
  filter(term != "(Intercept)") %>%
  mutate(term = reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Regression Coefficients",
    subtitle = paste("R² =", round(model_glance$r.squared, 3)),
    x = "Economic Indicator", 
    y = "Effect on Leasing Volume"
  )


library(ggplot2)
library(dplyr)

max_leased <- max(quarterly_combined$total_leased_sf, na.rm = TRUE)
max_sp500 <- max(econ_indicators$S.P.500, na.rm = TRUE)
scaling_factor <- max_leased / max_sp500

p <- ggplot() +
  geom_col(data = quarterly_combined, 
           aes(x = date_value, y = total_leased_sf), 
           fill = "darkblue", alpha = 0.7) +
  
  geom_line(data = econ_indicators %>% 
              filter(!is.na(S.P.500)), 
            aes(x = date, y = S.P.500 * scaling_factor), 
            color = "red", size = 1) +
  
  scale_y_continuous(
    name = "Total Leased Space (sq ft)",
    sec.axis = sec_axis(~./scaling_factor, name = "S&P 500 Index")
  ) +
  
  scale_x_date(
    name = "Year-Quarter",
    breaks = quarterly_combined$date_value,
    labels = paste(quarterly_combined$year, quarterly_combined$quarter)
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "S&P 500 Index vs. Commercial Leasing Volume")

print(p)



library(ggplot2)
library(dplyr)

quarterly_pct_change <- quarterly_combined %>%
  arrange(year, quarter) %>%
  mutate(
    pct_change = (total_leased_sf / lag(total_leased_sf) - 1) * 100
  ) %>%
  filter(!is.na(pct_change))

sp500_filtered <- econ_indicators %>% 
  filter(!is.na(S.P.500)) %>%
  filter(date >= min(quarterly_pct_change$date_value) &
           date <= max(quarterly_pct_change$date_value))

min_sp500 <- min(sp500_filtered$S.P.500, na.rm = TRUE)
max_sp500 <- max(sp500_filtered$S.P.500, na.rm = TRUE)
mid_sp500 <- (min_sp500 + max_sp500) / 2

max_abs_pct <- max(abs(quarterly_pct_change$pct_change), na.rm = TRUE)
min_pct <- -max_abs_pct
max_pct <- max_abs_pct

sp500_range <- max_sp500 - min_sp500
scaling_factor <- (max_pct - min_pct) / sp500_range
offset <- -mid_sp500 * scaling_factor 

p <- ggplot() +
  geom_col(data = quarterly_pct_change, 
           aes(x = date_value, y = pct_change, fill = pct_change >= 0), 
           alpha = 0.8) +
  
  geom_line(data = sp500_filtered, 
            aes(x = date, y = S.P.500 * scaling_factor + offset), 
            color = "gray20", size = 1) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  scale_fill_manual(values = c("darkred", "darkgreen"), 
                    labels = c("Decrease", "Increase"),
                    name = "Quarter Change") +
  
  scale_y_continuous(
    name = "Quarter-to-Quarter Change (%)",
    limits = c(min_pct * 1.1, max_pct * 1.1),
    sec.axis = sec_axis(~(. - offset)/scaling_factor, 
                        name = "S&P 500 Index",
                        breaks = seq(floor(min_sp500/100)*100, 
                                     ceiling(max_sp500/100)*100, 
                                     by = 500))
  ) +
  
  scale_x_date(
    name = "Year-Quarter",
    breaks = quarterly_pct_change$date_value,
    labels = paste(quarterly_pct_change$year, quarterly_pct_change$quarter)
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Quarter-to-Quarter Change in Leasing Volume vs. S&P 500"
  )

print(p)




library(ggplot2)
library(dplyr)

create_indicator_plot <- function(indicator_name, indicator_column, y_axis_label, color = "red") {
  indicator_data <- econ_indicators %>% 
    filter(!is.na(!!sym(indicator_column))) %>%
    select(date, !!sym(indicator_column))
  
  max_leased <- max(quarterly_combined$total_leased_sf, na.rm = TRUE)
  max_indicator <- max(indicator_data[[indicator_column]], na.rm = TRUE)
  scaling_factor <- max_leased / max_indicator
  
  p <- ggplot() +
    geom_col(data = quarterly_combined, 
             aes(x = date_value, y = total_leased_sf), 
             fill = "darkblue", alpha = 0.7) +
    
    geom_line(data = indicator_data, 
              aes(x = date, y = !!sym(indicator_column) * scaling_factor), 
              color = color, size = 1) +
    
    scale_y_continuous(
      name = "Total Leased Space (sq ft)",
      sec.axis = sec_axis(~./scaling_factor, name = y_axis_label)
    ) +
    
    scale_x_date(
      name = "Year-Quarter",
      breaks = quarterly_combined$date_value,
      labels = paste(quarterly_combined$year, quarterly_combined$quarter)
    ) +
    
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste(indicator_name, "vs. Commercial Leasing Volume"))
  
  return(p)
}

create_pct_change_plot <- function(indicator_name, indicator_column, y_axis_label, color = "gray20") {
  quarterly_pct_change <- quarterly_combined %>%
    arrange(year, quarter) %>%
    mutate(
      pct_change = (total_leased_sf / lag(total_leased_sf) - 1) * 100
    ) %>%
    filter(!is.na(pct_change))
  
  indicator_data <- econ_indicators %>% 
    filter(!is.na(!!sym(indicator_column))) %>%
    select(date, year, quarter, !!sym(indicator_column))
  
  indicator_filtered <- indicator_data %>%
    filter(date >= min(quarterly_pct_change$date_value) &
             date <= max(quarterly_pct_change$date_value))
  
  min_indicator <- min(indicator_filtered[[indicator_column]], na.rm = TRUE)
  max_indicator <- max(indicator_filtered[[indicator_column]], na.rm = TRUE)
  mid_indicator <- (min_indicator + max_indicator) / 2
  
  max_abs_pct <- max(abs(quarterly_pct_change$pct_change), na.rm = TRUE)
  min_pct <- -max_abs_pct
  max_pct <- max_abs_pct
  
  indicator_range <- max_indicator - min_indicator
  scaling_factor <- (max_pct - min_pct) / indicator_range
  offset <- -mid_indicator * scaling_factor
  
  p <- ggplot() +
    geom_col(data = quarterly_pct_change, 
             aes(x = date_value, y = pct_change, fill = pct_change >= 0), 
             alpha = 0.8) +
    
    geom_line(data = indicator_filtered, 
              aes(x = date, y = !!sym(indicator_column) * scaling_factor + offset), 
              color = color, size = 1) +
    
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    
    scale_fill_manual(values = c("darkred", "darkgreen"), 
                      labels = c("Decrease", "Increase"),
                      name = "Quarter Change") +
    
    scale_y_continuous(
      name = "Quarter-to-Quarter Change (%)",
      limits = c(min_pct * 1.1, max_pct * 1.1),
      sec.axis = sec_axis(~(. - offset)/scaling_factor, 
                          name = y_axis_label)
    ) +
    
    scale_x_date(
      name = "Year-Quarter",
      breaks = quarterly_pct_change$date_value,
      labels = paste(quarterly_pct_change$year, quarterly_pct_change$quarter)
    ) +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    ) +
    labs(
      title = paste("Quarter-to-Quarter Change in Leasing Volume vs.", indicator_name)
    )
  
  return(p)
}


treasury_plot <- create_indicator_plot(
  "10-Year Treasury Rate", 
  "X10Y.Treasury.Rate", 
  "10-Year Treasury Rate (%)", 
  "green"
)
print(treasury_plot)

treasury_pct_plot <- create_pct_change_plot(
  "10-Year Treasury Rate", 
  "X10Y.Treasury.Rate", 
  "10-Year Treasury Rate (%)", 
  "green"
)
print(treasury_pct_plot)


fedfunds_plot <- create_indicator_plot(
  "Federal Funds Rate", 
  "Fed.Funds.Rate", 
  "Federal Funds Rate (%)", 
  "purple"
)
print(fedfunds_plot)

fedfunds_pct_plot <- create_pct_change_plot(
  "Federal Funds Rate", 
  "Fed.Funds.Rate", 
  "Federal Funds Rate (%)", 
  "black"
)
print(fedfunds_pct_plot)

cpi_plot <- create_indicator_plot(
  "Consumer Price Index", 
  "Consumer.Price.Index..CPI.", 
  "CPI", 
  "orange"
)
print(cpi_plot)

cpi_pct_plot <- create_pct_change_plot(
  "Consumer Price Index", 
  "Consumer.Price.Index..CPI.", 
  "CPI", 
  "orange"
)
print(cpi_pct_plot)

unemployment_plot <- create_indicator_plot(
  "Unemployment Rate", 
  "Unemployment.Rate", 
  "Unemployment Rate (%)", 
  "brown"
)
print(unemployment_plot)

unemployment_pct_plot <- create_pct_change_plot(
  "Unemployment Rate", 
  "Unemployment.Rate", 
  "Unemployment Rate (%)", 
  "brown"
)
print(unemployment_pct_plot)

gdp_plot <- create_indicator_plot(
  "GDP", 
  "GDP", 
  "GDP (Billions USD)", 
  "blue"
)
print(gdp_plot)

gdp_pct_plot <- create_pct_change_plot(
  "GDP", 
  "GDP", 
  "GDP (Billions USD)", 
  "blue"
)
print(gdp_pct_plot)




covid_weekly = read.csv("covid.csv")
covid_weekly$End.Week = as.Date(covid_weekly$End.Week)
covid_weekly <- covid_weekly %>%
  rename(week = End.Week, cases = COVID.19.Deaths) %>%
  filter(week >= start_date & week <= end_date)
library(ggplot2)
library(dplyr)

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2023-06-30")

quarterly_combined <- quarterly_combined %>%
  filter(date_value >= start_date & date_value <= end_date)

covid_weekly <- covid_weekly %>%
  filter(week >= start_date & week <= end_date)

max_leased <- max(quarterly_combined$total_leased_sf, na.rm = TRUE)
max_cases <- max(covid_weekly$cases, na.rm = TRUE)
scaling_factor <- max_leased / max_cases

ggplot() +
  geom_col(data = quarterly_combined,
           aes(x = date_value, y = total_leased_sf),
           fill = "darkblue", alpha = 0.7) +
  
  geom_line(data = covid_weekly,
            aes(x = week, y = cases * scaling_factor),
            color = "firebrick", size = 1) +
  
  scale_y_continuous(
    name = "Total Leased Space (sq ft)",
    sec.axis = sec_axis(~./scaling_factor, name = "Weekly COVID-19 Cases")
  ) +
  
  scale_x_date(
    name = "Year-Quarter",
    breaks = quarterly_combined$date_value,
    labels = paste(quarterly_combined$year, quarterly_combined$quarter)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Weekly COVID-19 Cases vs. Commercial Leasing Volume")


quarterly_pct_change <- quarterly_combined %>%
  arrange(year, quarter) %>%
  mutate(pct_change = (total_leased_sf / lag(total_leased_sf) - 1) * 100) %>%
  filter(!is.na(pct_change))

covid_filtered <- covid_weekly %>%
  filter(week >= min(quarterly_pct_change$date_value) & 
           week <= max(quarterly_pct_change$date_value))

min_cases <- min(covid_filtered$cases, na.rm = TRUE)
max_cases <- max(covid_filtered$cases, na.rm = TRUE)
mid_cases <- (min_cases + max_cases) / 2

max_abs_pct <- max(abs(quarterly_pct_change$pct_change), na.rm = TRUE)
min_pct <- -max_abs_pct
max_pct <- max_abs_pct

case_range <- max_cases - min_cases
scaling_factor <- (max_pct - min_pct) / case_range
offset <- -mid_cases * scaling_factor

ggplot() +
  geom_col(data = quarterly_pct_change,
           aes(x = date_value, y = pct_change, fill = pct_change >= 0),
           alpha = 0.8) +
  
  geom_line(data = covid_filtered,
            aes(x = week, y = cases * scaling_factor + offset),
            color = "gray20", size = 1) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  scale_fill_manual(values = c("darkred", "darkgreen"),
                    labels = c("Decrease", "Increase"),
                    name = "Quarter Change") +
  
  scale_y_continuous(
    name = "Quarter-to-Quarter Change (%)",
    limits = c(min_pct * 1.1, max_pct * 1.1),
    sec.axis = sec_axis(~(. - offset)/scaling_factor,
                        name = "Weekly COVID-19 Deaths",
                        breaks = seq(0, max_cases, by = 5000))
  ) +
  
  scale_x_date(
    name = "Year-Quarter",
    breaks = quarterly_pct_change$date_value,
    labels = paste(quarterly_pct_change$year, quarterly_pct_change$quarter)
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(title = "Quarter-to-Quarter Change in Leasing Volume vs. COVID-19 Deaths")

