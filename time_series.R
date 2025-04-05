Leases$market = as.factor(Leases$market)

Leases$transaction_type = as.factor(Leases$transaction_type)

 

MMOD = Major_Market_Occupancy_Data_revised

MMOD$market = as.factor(MMOD$market)

 

library(ggplot2)

library(dplyr)

 

PAD = Price_and_Availability_Data

 

PAD = PAD %>%

  mutate(year_quarter = paste0(year, quarter))

 

levels(PAD$market)

levels(MMOD$market)

 

 

target_cities <- c( "Manhattan", "Chicago", "Philadelphia", "Houston", "San Francisco", "Los Angeles", "Dallas", "Austin", "Washington DC", "San Jose" )

 

PAD_filtered <- PAD %>%

  filter(market %in% target_cities)

PAD_filtered$market = as.factor(PAD_filtered$market)

 

 

 

ggplot(PAD_filtered, aes(x = year_quarter, y = availability_proportion, color = market, linetype = internal_class)) +

  geom_line() +

  labs(title = "Volatility in Availible Space", x = "Year", y = "Availiblity") +

  theme_minimal()

 

 

ggplot(PAD_filtered, aes(x = year_quarter, y = availability_proportion, color = market, group = interaction(market, internal_class), linetype = internal_class)) +

  geom_line()+

  geom_point()+

  labs(title = "Availability Proportion by Internal Class")+

  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

 

 

library("forecast")

nQ = (2024 - 2018+1)*4

ts_PADF = ts(PAD_filtered$availability_proportion[1:nQ], start = c(2018, 1), frequency = 4)

plot(ts_PADF)

TsFit = auto.arima(ts_PADF)

 

forecast_values = forecast(TsFit, h = 8)

plot(forecast_values,

     main = "Forecast for Availability Proportion",

     xlab = "Time", ylab = "AP",

     col = "blue")