#### Preamble ####
# Purpose: Simulate a dataset resembling the Broadway Gross dataset 
  #by generating plausible values that could realistically appear within it.
# Author: Xuanle Zhou
# Date: 23 November 2024
# Contact: isabella.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: Construct the overview of how the dataset should look like.
# Any other information needed? Make sure you are in the `Broadway_Gross` rproj


#### Workspace setup ####
library(tidyverse)
set.seed(853)

num_rows <- 10000 

#### Simulate data ####
# Create the Year and Month columns
Year <- sample(seq(2000, 2020, length.out = 8), num_rows, replace = TRUE)
Month <- sample(c("January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", 
                  "December"),
                num_rows, replace = TRUE)

# Create a base dataset
simulated_data <- tibble(Year = Year, Month = Month)

# Define the number of weeks for each month in a single year
weeks_in_month <- c(4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5)
start_week <- cumsum(c(0, weeks_in_month[-length(weeks_in_month)])) + 1

# Map months to their week ranges
month_week_map <- tibble(
  Month = c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"),
  StartWeek = start_week,
  NumWeeks = weeks_in_month
)

# Assign week numbers to each row
simulated_data <- simulated_data %>%
  left_join(month_week_map, by = "Month") %>%
  rowwise() %>%
  mutate(week_number = sample(seq(StartWeek, StartWeek + NumWeeks - 1), 1)) %>%
  ungroup() %>%
  select(Year, Month, week_number)

# Simulate average_ticket_price, seats_sold, performance, and weekly_gross
simulated_data <- simulated_data %>%
  mutate(
    average_ticket_price = rnorm(n = nrow(simulated_data), mean = 250, 
                                 sd = 100) %>% pmax(5) %>% pmin(500),  # Range: 5–500
    seats_in_theatre = rnorm(n = nrow(simulated_data), mean = 1250, 
                       sd = 500) %>% pmax(50) %>% pmin(2000),    # Range: 50–2000
    performance = rnorm(n = nrow(simulated_data), mean = 10, 
                        sd = 5) %>% pmax(1) %>% pmin(20),              # Range: 1–20
    weekly_gross = rnorm(n = nrow(simulated_data), mean = 1700000, 
                         sd = 500000) %>% pmax(100) %>% pmin(3400000)  # Range: 100–3400000
  )

# Holiday 
simulated_data <- simulated_data %>%
  mutate(
    holiday_week = if_else(
      week_number %in% c(1, 27, 28, 36, 47, 48, 52),
      1,
      0
    )
  )

# Tony Awards
simulated_data <- simulated_data %>%
  mutate(
    Tony_Award = if_else(
      (Month == "June" & Year != 2020) | (Month == "September" & Year == 2020),
      1,
      0
    )
  )

#### Save data ####
write_csv(simulated_data, "data/00-simulated_data/simulated_data.csv")
