library(tidyverse)
library(lubridate)
library(scales)
library(forecast)
library(zoo)

Leases = read.csv("~/Downloads/Leases.csv")
Leases$market = as.factor(Leases$market)

target_market = c(
  "Manhattan", "Chicago", "Philadelphia", "Houston",
  "San Francisco", "Los Angeles", "Dallas/Ft Worth", "Austin",
  "Washington DC", "South Bay/San Jose"
)

Leases = Leases %>%
  filter(market %in% target_market) %>%
  mutate(
    quarter_num = as.numeric(str_extract(quarter, "\\d")),
    date = as.Date(paste(year, (quarter_num - 1) * 3 + 1, "01", sep = "-")),
    quarter_label = paste(year, paste0("Q", quarter_num))
  )

lease_trend = Leases %>%
  group_by(market, date, quarter_label) %>%
  summarise(total_leased_sf = sum(leasedSF, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

quarter_labels = lease_trend %>%
  distinct(date, quarter_label) %>%
  arrange(date)

plot_lease = ggplot(lease_trend, aes(x = date, y = total_leased_sf, color = market)) +
  geom_line(size = 1) +
  scale_x_date(
    breaks = quarter_labels$date,
    labels = quarter_labels$quarter_label,
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Total Leased Square Feet Over Time by Market",
    x = "Quarter",
    y = "Total Leased SF",
    color = "Market"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

rba_trend = Leases %>%
  group_by(market, date, quarter_label) %>%
  summarise(total_rba = sum(RBA, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

rba_trend_filtered = rba_trend %>%
  group_by(market) %>%
  filter(n_distinct(total_rba) > 1) %>%
  ungroup()

plot_rba = ggplot(rba_trend_filtered, aes(x = date, y = total_rba)) +
  geom_line(color = "black", size = 1) +
  facet_wrap(~ market, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Total RBA Over Time by Market",
    x = NULL,
    y = "Total RBA (Rentable Building Area)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

manhattan_leases = Leases %>%
  filter(market == "Manhattan") %>%
  group_by(date, quarter_label) %>%
  summarise(overall_rent = mean(overall_rent, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

ts_rent = ts(manhattan_leases$overall_rent, start = c(year(min(manhattan_leases$date)), quarter(min(manhattan_leases$date))), frequency = 4)

model = auto.arima(ts_rent)
forecast_rent = forecast(model, h = 8, level = c(50, 80))

forecast_df = data.frame(
  date = seq(max(manhattan_leases$date) + months(3), by = "quarter", length.out = 8),
  fit = as.numeric(forecast_rent$mean),
  lower_80 = as.numeric(forecast_rent$lower[, "80%"]),
  upper_80 = as.numeric(forecast_rent$upper[, "80%"]),
  lower_50 = as.numeric(forecast_rent$lower[, "50%"]),
  upper_50 = as.numeric(forecast_rent$upper[, "50%"])
) %>%
  mutate(
    quarter_label = paste0(year(date), " Q", quarter(date))
  )

plot_df = bind_rows(
  manhattan_leases %>% mutate(type = "Actual"),
  forecast_df %>% rename(overall_rent = fit) %>% mutate(type = "Forecast")
)

plot_forecast = ggplot() +
  geom_ribbon(data = forecast_df, aes(x = date, ymin = lower_80, ymax = upper_80), fill = "lightblue", alpha = 0.4) +
  geom_ribbon(data = forecast_df, aes(x = date, ymin = lower_50, ymax = upper_50), fill = "blue", alpha = 0.3) +
  geom_line(data = plot_df, aes(x = date, y = overall_rent, color = type), size = 1) +
  scale_x_date(breaks = plot_df$date, labels = plot_df$quarter_label) +
  scale_y_continuous(labels = dollar_format(prefix = "$", accuracy = 1)) +
  labs(
    title = "Average Overall Rent Forecast (Manhattan)",
    x = "Quarter",
    y = "Average Rent (Dollars)",
    color = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9)
  )

print(plot_lease)
print(plot_rba)
print(plot_forecast)
