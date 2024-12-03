# Broadway's Gross Analysis 

## Overview

This repository contains the code, data, and analysis conducted in R to examine factors influencing Broadway's weekly gross revenue using data provided by Playbill. A multiple linear regression model is utilized to explore how variables such as ticket pricing, theater capacity, performance frequency, and time-related factors impact Broadway gross revenue.

To use this repository, click the green "Code" button and select "Download ZIP." Move the downloaded folder to your desired location on your computer and modify it as needed for your work.

## File Structure

The repo is structured as:

-   `data/simulated_data`: contains simulated datasets.
-   `data/raw_data` the raw data obtained from https://github.com/tacookson/data/blob/master/broadway-grosses/grosses.csv.
-   `data/analysis_data` contains the cleaned datasets, including the training and testing datasets used in the modeling process.
-   `model` contains the RDS file for the fitted model.
-   `other` contains datasheets, relevant literature, documentation of LLM usage, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts for tasks such as downloading data, simulating data, testing simulated data, cleaning data, test analysis data, performing exploratory data analysis, and building the model.


## Statement on LLM usage

Aspects of the code and paper were written with the help of ChatGPT 4o. Some of the data interpretation, introduction, abstract and discussion were also written using ChatGPT 4o. The entire chat history is available in `other/llm_usage/usage.txt`
