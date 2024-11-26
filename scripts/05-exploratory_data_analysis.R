#### Preamble ####
# Purpose: Do exploratory data analyis on the cleaned Broadway grosses data
# Author: Xuanle Zhou
# Date: 24 November 2024
# Contact: isabella.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: Retrieve the cleaned Broadway grosses data located in 
  #the 02-analysis_data folder.
# Any other information needed? NA


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(ggplot2)
library(scales) 
library(dplyr)

#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/cleaned_broadway_grosses.parquet")
colnames(analysis_data)

### EDA ####

# Generate a histogram for Weekly Gross Revenue
ggplot(analysis_data, aes(x = weekly_gross)) +
  geom_histogram(binwidth = 100000, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Weekly Gross Revenue",
    x = "Weekly Gross Revenue ($)",
    y = "Number of Weeks"
  ) +
  scale_x_continuous(labels = comma) +  # Format x-axis labels with commas
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Generate a dot chart for weekly gross change for each observations
ggplot(analysis_data, aes(x = week_ending, y = weekly_gross)) +
  geom_point(color = "steelblue", size = 0.5, alpha = 0.7) +  # Replace geom_dot with geom_point
  labs(
    x = "Year",
    y = "Weekly Gross ($)",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  ) +
  scale_x_date(
    date_labels = "%Y",       # Show the year on the x-axis
    date_breaks = "1 year"    # Add a tick for each year
  ) +
  scale_y_continuous(labels = comma)  # Format y-axis labels with commas


# Aggregate by month
data_monthly <- analysis_data %>%
  mutate(month = floor_date(week_ending, "month")) %>%
  group_by(month) %>%
  summarise(average_weekly_gross = mean(weekly_gross, na.rm = TRUE))

# Create the plot
ggplot(data_monthly, aes(x = month, y = average_weekly_gross)) +
  geom_line(color = "blue", size = 1) + 
  labs(
    title = "Average Weekly Gross Revenue Over Time",
    subtitle = "Monthly trends of weekly gross revenue",
    x = "Date",
    y = "Average Weekly Gross Revenue ($)"
  ) +
  theme_minimal() +
  scale_x_date(
    date_labels = "%Y-%b",   # Show year and month (e.g., "2024-Jan")
    date_breaks = "6 months" # Set ticks at 6-month intervals
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )


# Facet the plot by Year

data_monthly <- data_monthly %>%
  mutate(
    year = year(month),                        # Extract year
    month_label = month(month, label = TRUE)  # Extract month name (label)
  )

# Create the faceted plot
ggplot(data_monthly, aes(x = month_label, y = average_weekly_gross, group = year)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Average Weekly Gross Revenue Over Time (Faceted by Year)",
    x = "Month",
    y = "Average Weekly Gross Revenue ($)"
  ) +
  theme_minimal() +
  facet_wrap(~ year, ncol = 3) + # Facet by year, 3 columns
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

data_monthly <- data_monthly %>%
  mutate(year = as.factor(year))  # Convert year to a factor

# Generate the integrated chart with one line per year to see seasonal pattern
ggplot(data_monthly, aes(x = month_label, y = average_weekly_gross, color = year, group = year)) +
  geom_line(size = 1) +   # Plot lines for each year
  scale_color_viridis_d(option = "plasma", direction = -1)+ #Use Viridis color scale with reversed direction
  labs(
    title = "Average Weekly Gross Revenue Over Time",
    subtitle = "Integrated chart with one line per year",
    x = "Month",
    y = "Average Weekly Gross Revenue ($)",
    color = "Year"  # Legend title for the color scale
  ) +
  theme_minimal() +  # Clean, minimalistic theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Center and format title
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Center and format subtitle
    legend.position = "right"  # Place legend on the right
  )

# Generate a histogram for avg_ticket_price 
ggplot(analysis_data, aes(x = avg_ticket_price)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black", alpha = 0.7) +  # Customize bin width and colors
  labs(
    title = "Distribution of Average Ticket Price",
    x = "Average Ticket Price ($)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Scatter plot for avg_ticket_price and Weekly Gross
ggplot(analysis_data, aes(x = avg_ticket_price, y = weekly_gross)) +
  geom_point(color = "steelblue", size = 0.5, alpha = 0.8) +  
  scale_y_continuous(labels = comma) +  
  labs(
    title = "Scatter Plot of Average Ticket Price vs Weekly Gross",
    x = "Average Ticket Price ($)",
    y = "Weekly Gross ($)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Generate a histogram for seats_in_theatre
ggplot(analysis_data, aes(x = seats_in_theatre)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "black", alpha = 0.7) +  
  labs(
    title = "Distribution of Seats in the Theatre",
    x = "Number of Seats in the Theatre",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Scatter plot for seats_in_theatre and Weekly Gross
ggplot(analysis_data, aes(x = seats_in_theatre, y = weekly_gross)) +
  geom_point(color = "steelblue", size = 0.5, alpha = 0.8) +  
  scale_y_continuous(labels = comma) +  
  labs(
    title = "Scatter Plot of Average Ticket Price vs Weekly Gross",
    x = "Average Ticket Price ($)",
    y = "Weekly Gross ($)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Generate a histogram for performances
ggplot(analysis_data, aes(x = performances)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) + 
  scale_x_continuous(breaks = seq(0, max(analysis_data$performances, na.rm = TRUE), by = 1)) +  
  labs(
    title = "Distribution of Performances",
    x = "Number of Performances",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Scatter plot for performances and Weekly Gross
ggplot(analysis_data, aes(x = performances, y = weekly_gross)) +
  geom_point(color = "steelblue", size = 0.5, alpha = 0.8) +  
  scale_y_continuous(labels = comma) +  
  labs(
    title = "Scatter Plot of Average Ticket Price vs Weekly Gross",
    x = "Average Ticket Price ($)",
    y = "Weekly Gross ($)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Comparing Holiday and Non-Holiday Weeks
# Convert holiday_week to a factor
analysis_data <- analysis_data %>%
  mutate(holiday_week = as.factor(holiday_week))  # Ensure holiday_week is treated as a categorical var
# Aggregate data to calculate average weekly gross for holiday and non-holiday weeks
aggregated_data_holiday <- analysis_data %>%
  group_by(holiday_week) %>%  # Group by holiday status
  summarise(avg_weekly_gross = mean(weekly_gross, na.rm = TRUE)) %>%  # Calculate average Weekly Gross
  mutate(holiday_week = case_when(
    holiday_week == 0 ~ "Non-Holiday Week",  # Rename 0 as "Non-Holiday Week"
    holiday_week == 1 ~ "Holiday Week"      # Rename 1 as "Holiday Week"
  ))

# Bar chart for average weekly gross
ggplot(aggregated_data_holiday, aes(x = holiday_week, y = avg_weekly_gross, fill = holiday_week)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +  # Bar chart for aggregated data
  scale_fill_manual(values = c("darkred", "darkblue")) +  # Custom colors for holiday and non-holiday
  labs(
    title = "Average Weekly Gross for Holiday vs Non-Holiday Weeks",
    x = "Week Type",
    y = "Average Weekly Gross ($)",
    fill = "Holiday Week"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Add commas to the y-axis for readability
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"  # Remove legend since x-axis already specifies week types
  )

# Aggregate data to calculate average weekly gross for Tony Award Period and off Period
aggregated_data_award <- analysis_data %>%
  group_by(Tony_Award) %>%  # Group by Tony Award status
  summarise(avg_weekly_gross = mean(weekly_gross, na.rm = TRUE)) %>%  # Calculate average Weekly Gross
  mutate(Tony_Award = case_when(
    Tony_Award == 0 ~ "Off-Award Month",     # Rename 0 as "Off-Award Month"
    Tony_Award == 1 ~ "Tony Award Month"    # Rename 1 as "Tony Award Month"
  ))

# Bar chart for average weekly gross
ggplot(aggregated_data_award, aes(x = Tony_Award, y = avg_weekly_gross, fill = Tony_Award)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +  # Bar chart for aggregated data
  scale_fill_manual(values = c("darkred", "darkblue")) +  # Custom colors for holiday and non-holiday
  labs(
    title = "Average Weekly Gross for Holiday vs Non-Holiday Weeks",
    x = "Week Type",
    y = "Average Weekly Gross ($)",
    fill = "Holiday Week"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Add commas to the y-axis for readability
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"  # Remove legend since x-axis already specifies week types
  )