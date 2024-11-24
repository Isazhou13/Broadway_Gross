#### Preamble ####
# Purpose: Models for Broadway grosses data
# Author: Xuanle Zhou
# Date: 24 November 2024
# Contact: isabella.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: Retrieve training data generated in 02-analysis_data
# Any other information needed? NA


#### Workspace setup ####
library(tidyverse)
library(rstanarm)

set.seed(853)

#### Read data ####
train_data <- read_parquet("data/02-analysis_data/broadway_grosses_train_data.parquet")

### Model data ####

regression_model <- lm(weekly_gross ~ avg_ticket_price + seats_in_theatre + 
                         performances + holiday_week + Tony_Award + year, 
                       data = train_data)

#### Save model ####
saveRDS(regression_model, file = "models/regression_model.rds")

