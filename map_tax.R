library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(gridExtra)
library(scales)

tax_data <- read.csv("tax.csv")
lease = read.csv("Leases.csv")

leasing_q1_2020 <- lease %>%
  filter(year == 2020 & quarter == "Q1") %>%
  group_by(market) %>%
  summarize(leasing_2020_q1 = sum(leasedSF, na.rm = TRUE))

leasing_q2_2023 <- lease %>%
  filter(year == 2023 & quarter == "Q2") %>%
  group_by(market) %>%
  summarize(leasing_2023_q2 = sum(leasedSF, na.rm = TRUE))

leasing_change <- left_join(leasing_q1_2020, leasing_q2_2023, by = "market") %>%
  mutate(percent_change = ((leasing_2023_q2 - leasing_2020_q1) / leasing_2020_q1) * 100)

market_to_state <- lease %>%
  select(market, state) %>%
  distinct()

selected_markets <- c("Atlanta", "Boston", "Chicago", "Dallas/Ft Worth", "Denver", 
                      "Los Angeles", "Manhattan", "San Francisco", "Seattle")


state_mapping <- data.frame(
  abbr = c(state.abb, "DC"),
  full_name = c(state.name, "district of columbia")
)

market_locations <- data.frame(
  market = selected_markets,
  abbr = market_to_state$state[match(selected_markets, market_to_state$market)]
) %>%
  left_join(state_mapping, by = "abbr") %>%
  mutate(
    state = full_name,
    lat = c(33.749, 42.3601, 41.8781, 32.7767, 39.7392, 34.0522, 40.7831, 37.7749, 47.6062),
    long = c(-84.388, -71.0589, -87.6298, -96.797, -104.9903, -118.2437, -73.9712, -122.4194, -122.3321)
  ) %>%
  select(market, state, lat, long)

market_locations <- left_join(market_locations, leasing_change %>% select(market, percent_change), by = "market")
create_map <- function(year) {
  year_col <- paste0("tax_per_capita_", year)
  
  state_data <- tax_data %>%
    select(state, !!sym(year_col)) %>%
    rename(tax_per_capita = !!sym(year_col))
  
  states_map <- map_data("state")
  
  state_data$state_name <- tolower(state_data$state)
  
  map_data <- left_join(states_map, state_data, by = c("region" = "state_name"))
  
  p <- ggplot() +
    geom_polygon(data = map_data, 
                 aes(x = long, y = lat, group = group, fill = tax_per_capita),
                 color = "white", size = 0.2) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    scale_fill_gradient(name = "Tax per capita ($)",
                        labels = dollar_format(),
                        low = "dodgerblue", high = "darkblue") +
    labs(title = paste("State Tax per Capita (",year,") and Commercial Leasing Change"),
         caption = "Arrow direction: Up = increase, Down = decrease") +
    theme_minimal() +
    theme(legend.position = "right",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
  
  market_with_data <- market_locations %>%
    filter(state %in% tax_data$state)

  p <- p + 
    geom_segment(data = market_with_data,
                 aes(x = long, y = lat,
                     xend = long,
                     yend = lat + ifelse(percent_change > 0, 1.5, -1.5),
                     color = percent_change,
                     size = abs(percent_change)),
                 arrow = arrow(length = unit(0.25, "cm"), type="closed",angle=45,ends="last"),
                 alpha = 1.0) +
    scale_color_gradient2(name = "% Change in Leasing\n(Q1 2020 - Q2 2023)",
                          low = "firebrick2", mid = "white", high = "forestgreen", 
                          midpoint = 0) +
    scale_size_continuous(name = "Magnitude of Change (%)",
                          range = c(0.5, 3)) +
    geom_text(data = market_with_data,
              aes(x = long, y = lat + ifelse(percent_change > 0, 0, -2), label = market),
              size = 3.5, check_overlap = TRUE, fontface = "bold")
  
  return(p)
}

create_map(2020)
create_map(2021)
create_map(2022)
create_map(2023)
