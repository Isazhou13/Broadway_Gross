#### Preamble ####
# Purpose: Cleans the raw Broadway Gross dataset
# Author: Xuanle Zhou
# Date: 23 November 2024
# Contact: isabella.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: NA
# Any other information needed? Make sure you are in the `Broadway_Gross` rproj

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(arrow)
library(rsample)


#### Clean data ####
cleaned_data <- read_csv("data/01-raw_data/grosses_raw.csv")
colnames(cleaned_data)

cleaned_cpi_data <- read_csv("data/01-raw_data/cpi_raw.csv")
colnames(cleaned_cpi_data)


cleaned_data <- cleaned_data %>%
  # Create separate columns for year, month, and day without removing the original column
  mutate(
    year = as.integer(substr(week_ending, 1, 4)),       # Extract year
    month = as.integer(substr(week_ending, 6, 7)),     # Extract month
    day = as.integer(substr(week_ending, 9, 10))       # Extract day
  ) %>%
  # Convert numeric month to month name
  mutate(
    month = month.name[month]  # Convert month to full name
  ) %>%
  select(-day)

cleaned_cpi_data <- cleaned_cpi_data %>%
  separate(year_month, into = c("year", "month"), sep = "-", convert = TRUE) %>%
  mutate(
    month = month.name[as.integer(month)]  # Convert numeric month to month name
  ) 

# Remove unnecessary columns from the dataset
cleaned_data <- cleaned_data %>%
  select(
    -weekly_gross_overall, 
    -show, 
    -theatre, 
    -potential_gross, 
    -top_ticket_price, 
    -seats_sold, 
    -pct_capacity, 
    -previews
  )

# Add holiday_week
cleaned_data <- cleaned_data %>%
  mutate(
    holiday_week = if_else(
      week_number %in% c(1, 27, 28, 36, 47, 48, 52), 
      1, 
      0
    )
  )

# Add Tony_Award 
cleaned_data <- cleaned_data %>%
  mutate(
    Tony_Award = if_else(
      (month == "June" & year != 2020) | (month == "September" & year == 2020),
      1,
      0
    )
  )
  
# Remove rows where any of the specified columns are 0
cleaned_data <- cleaned_data %>%
  filter(
    weekly_gross != 0,
    seats_in_theatre != 0,
    avg_ticket_price != 0,
    performances != 0
  )

# Remove rows with any NA values
cleaned_data <- cleaned_data %>%
  drop_na()

#  Include only data from the year 2000 onward.
cleaned_data <- cleaned_data %>% filter(year >= 2010 & year < 2020 )

# Split the data into training (80%) and testing (20%)
set.seed(123)  # Set seed for reproducibility
data_split <- initial_split(cleaned_data, prop = 0.8)

# Create training and testing datasets
train_data <- training(data_split)
test_data <- testing(data_split)

#### Save data ####
write_csv(train_data, "data/02-analysis_data/cleaned_broadway_grosses.csv")
write_parquet(train_data, "data/02-analysis_data/cleaned_broadway_grosses.parquet")

write_csv(train_data, "data/02-analysis_data/broadway_grosses_train_data.csv")
write_parquet(train_data, "data/02-analysis_data/broadway_grosses_train_data.parquet")

write_csv(test_data, "data/02-analysis_data/broadway_grosses_test_data.csv")
write_parquet(test_data, "data/02-analysis_data/broadway_grosses_test_data.parquet")

write_csv(cleaned_cpi_data, "data/02-analysis_data/cleaned_cpi_data.csv")
write_parquet(cleaned_cpi_data, "data/02-analysis_data/cleaned_cpi_data.parquet")