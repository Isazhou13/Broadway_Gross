#### Preamble ####
# Purpose: Tests the structure and values of the analysis data 
    #of Broadway Gross dataset is in the expected format
# Author: Xuanle Zhou
# Date: 24 November 2024
# Contact: isabella.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: 02-analysis_data.R must have been run
# Any other information needed? NA


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(here)

analysis_data <- read_csv(here("data", "02-analysis_data", "cleaned_broadway_grosses.csv"))

#### Test data ####

# Test if the data was successfully loaded
test_that("Test if the data was successfully loaded", {
  expect_true(exists("analysis_data"), 
              info = "Test Failed: The dataset could not be loaded.")
  message("Test Passed: The dataset was successfully loaded.")
})

#### Test data ####

# Test1:Check if the dataset has 10 rows
test_that("Dataset has the correct number of columns", {
  expected_columns <- 10
  actual_columns <- ncol(analysis_data)  # Get the actual number of columns
  expect_equal(actual_columns, expected_columns, 
               info = paste("Expected", expected_columns, "columns but got", actual_columns))
})

# Test2: Check if the dataset has missing values in columns

test_that("No missing values in critical columns", {
  expect_equal(sum(is.na(analysis_data$year)), 0)
  expect_equal(sum(is.na(analysis_data$month)), 0)
  expect_equal(sum(is.na(analysis_data$week_number)), 0)
  expect_equal(sum(is.na(analysis_data$avg_ticket_price)), 0)
  expect_equal(sum(is.na(analysis_data$seats_in_theatre)), 0)
  expect_equal(sum(is.na(analysis_data$performances)), 0)
  expect_equal(sum(is.na(analysis_data$weekly_gross)), 0)
  expect_equal(sum(is.na(analysis_data$holiday_week)), 0)
  expect_equal(sum(is.na(analysis_data$Tony_Award)), 0)
})

# Test3: Check if specific columns are of the correct type
test_that("Column types are correct", {
  expect_is(analysis_data$`year`, "numeric")
  expect_is(analysis_data$`month`, "character")
  expect_is(analysis_data$`week_number`, "numeric")
  expect_is(analysis_data$`avg_ticket_price`, "numeric")
  expect_is(analysis_data$`seats_in_theatre`, "numeric")
  expect_is(analysis_data$`performances`, "numeric")
  expect_is(analysis_data$`weekly_gross`, "numeric")
  expect_is(analysis_data$`holiday_week`, "numeric")
  expect_is(analysis_data$`Tony_Award`, "numeric")
})

#Test4: Check if columns Year are between 1985 and 2020
test_that("Year contains only values between 1985 and 2020", {
  expect_true(all(analysis_data$year >= 2000 & analysis_data$year <= 2020))
})

#Test5: Check if columns Month are in expected values 
expected_months <- c("January", "February", "March", "April", "May", "June",
                     "July", "August", "September",
                     "October", "November", "December")
test_that("Month Offence Occurred contains expected values", {
  unique_months <- unique(analysis_data$`month`)
  expect_setequal(
    unique_months, expected_months)
})

#Test6: Check if columns week number are in between 1 and 53
test_that("Week Number contains only values between 1 and 53", {
  expect_true(all(analysis_data$week_number >= 1 & analysis_data$week_number <= 53))
})

#Test7: Check if average_ticket_price, seats_sold, performance, and weekly_gross do not contain 0 values
test_that("average_ticket_price, seats_sold, performance, and weekly_gross do not contain 0 values", {
  expect_true(all(analysis_data$avg_ticket_price != 0))
  expect_true(all(analysis_data$seats_in_theatre != 0))
  expect_true(all(analysis_data$performances != 0))
  expect_true(all(analysis_data$weekly_gross != 0))
})

#Test8: Check if average_ticket_price, seats_sold, performance, and weekly_gross do not contain 0 values
test_that("average_ticket_price, seats_sold, performance, and weekly_gross do not contain 0 values", {
  expect_true(all(analysis_data$avg_ticket_price != 0))
  expect_true(all(analysis_data$seats_in_theatre != 0))
  expect_true(all(analysis_data$performances != 0))
  expect_true(all(analysis_data$weekly_gross != 0))
})

#Test9: Check if Holiday_Week and Tony_Award only contain 0 and 1
test_that("Holiday_Week and Tony_Award only contain 0 and 1", {
  expect_true(all(analysis_data$holiday_week %in% c(0, 1)))
  expect_true(all(analysis_data$Tony_Award %in% c(0, 1)))
})

#Test10: Check if Holiday_Week is correctly assigned for specific weeks
test_that("Holiday_Week is correctly assigned for specific weeks", {
  valid_holiday_weeks <- c(1, 27, 28, 36, 47, 48, 52)
  
  # Check if Holiday_Week is 1 for valid holiday weeks
  holiday_week_check <- analysis_data %>%
    filter(week_number %in% valid_holiday_weeks) %>%
    summarise(all_correct = all(holiday_week == 1)) %>%
    pull(all_correct)
  
  # Check if Holiday_Week is 0 for non-holiday weeks
  non_holiday_week_check <- analysis_data %>%
    filter(!week_number %in% valid_holiday_weeks) %>%
    summarise(all_correct = all(holiday_week == 0)) %>%
    pull(all_correct)
  
  # Assert both conditions
  expect_true(holiday_week_check)
  expect_true(non_holiday_week_check)
})

#Test11: Check if Tony_Award is correctly assigned based on the conditions
test_that("Tony_Award is correctly assigned based on the conditions", {
  # Check for correct assignment when Month is June and Year is not 2020
  june_check <- analysis_data %>%
    filter(month == "June" & year != 2020) %>%
    summarise(all_correct = all(Tony_Award == 1)) %>%
    pull(all_correct)
  
  # Check for correct assignment when Month is September and Year is 2020
  september_check <- analysis_data %>%
    filter(month == "September" & year == 2020) %>%
    summarise(all_correct = all(Tony_Award == 1)) %>%
    pull(all_correct)
  
  # Check for correct assignment for all other cases
  other_check <- analysis_data %>%
    filter(!((month == "June" & year != 2020) | (month == "September" & year == 2020))) %>%
    summarise(all_correct = all(Tony_Award == 0)) %>%
    pull(all_correct)
  
  # Assert all conditions
  expect_true(june_check)
  expect_true(september_check)
  expect_true(other_check)
})

