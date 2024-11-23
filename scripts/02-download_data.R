#### Preamble ####
# Purpose: Downloads and saves the data from GitHub
# Author: Xuanle Zhou
# Date: 23 November 2024
# Contact: isabella.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: NA
# Any other information needed? NA


#### Workspace setup ####
library(tidyverse)
library(readr)

#### Download data ####
cpi_url <- "https://raw.githubusercontent.com/tacookson/data/refs/heads/master/broadway-grosses/cpi.csv"
grosses_url <- "https://raw.githubusercontent.com/tacookson/data/refs/heads/master/broadway-grosses/grosses.csv"


#### Save data ####
cpi_raw <- read.csv(cpi_url)
write_csv(cpi_raw, "data/01-raw_data/cpi_raw.csv")

grosses_raw <- read.csv(grosses_url)
write_csv(grosses_raw, "data/01-raw_data/grosses_raw.csv")
