New version: 
library(dplyr)
library(ggplot2)
major_market=read.csv("major_market.csv")
major_market = major_market %>%
  mutate(change_in_occupancy=ending_occupancy_proportion - starting_occupancy_proportion
)

selected_market=c("Austin", "Houston", "San Francisco", "Los Angeles", "Dallas/Ft Worth", "South Bay/San Jose")

filtered_market=major_market %>%
  filter(market %in% selected_markets) %>%
  mutate(year_quarter=factor(paste(year,quarter, sep = "-"))
)

filtered_market=filtered_market %>%
  mutate(
    is_california=ifelse(market %in% c("Los Angeles", "San Francisco", "South Bay/San Jose"), "California", "Texas"),
    is_california=factor(is_california)
  )

custom_colors=c(
  "Los Angeles" = "#1f77b4",       
  "San Francisco" = "#6baed6",     
  "South Bay/San Jose" = "#9ecae1",
  "Austin" = "#d62728",            
  "Houston" = "#e6550d",           
  "Dallas/Ft Worth" = "#fdae6b"    
)

ggplot(filtered_market, aes(x = year_quarter, y = starting_occupancy_proportion, color = market, group = market)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Change in Occupancy Proportion Over Time",
    x = "Year-Quarter",
    y = "Change in Occupancy",
    color = "Market"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold")
  )

filtered_market=major_market %>%
  mutate(is_california = ifelse(market %in% c("Los Angeles", "San Francisco", "South Bay/San Jose"), "California", "Texas"),
          is_california = factor(is_california)
)

cor.test(major_market$is_california_numeric, major_market$starting_occupancy_proportion)