# Install tidyverse and load appropriate packages
install.packages("tidyverse")
library(dplyr)
library(tidyr)
library(lubridate)

# Upload and format data
data <- read.csv("Squat_Power_Data.csv")
data$date <- as.Date(data$date)
str(data)

# Count squats by type per athlete and filter out athlete/squat combos with less than 10 squats
filtered_data_with_count <- data %>%
  group_by(name, squat_type) %>%
  summarize(squat_count = n(), .groups = 'drop') %>%
  inner_join(data, by = c("name", "squat_type")) %>%
  filter(squat_count >= 10)

# Define function for estimated max per squat per athlete at 0.3 m/s
estimate_1rm <- function(data) {
  lm_model <- lm(velo_ms ~ load_lb, data = data)
  (0.3 - coef(lm_model)[1]) / coef(lm_model)[2]
}

# Define function for max power load
estimate_max_power_load <- function(data) {
  lm_model <- lm(power_w ~ poly(load_lb, 2, raw = TRUE), data = data)
  coef <- coef(lm_model)
  
  if (length(coef) < 3 || coef[3] >= 0) {
    return(NA)
  }
  
  -coef[2] / (2 * coef[3])
}

# Define function to calculate velocity at max power
predict_velocity_at_max_power <- function(data, max_power_load) {
  lm_model <- lm(velo_ms ~ load_lb, data = data)
  coef <- coef(lm_model)
  predicted_velocity <- coef[1] + coef[2] * max_power_load  # y = mx + b
  return(predicted_velocity)
}

# Add estimated 1RM and max power load to filtered data
combined_data <- filtered_data_with_count %>%
  group_by(name, squat_type) %>%
  summarize(
    estimated_1rm = estimate_1rm(pick(everything())),
    max_power_load = estimate_max_power_load(pick(everything())),
    .groups = 'drop'
  ) %>%
  left_join(filtered_data_with_count, by = c("name", "squat_type"))

# Remove athletes/squats with invalid power profiles
# Calclate max power percent 1RM and predict velocity at max power
cleaned_combined_data <- combined_data %>%
  filter(!is.na(max_power_load)) %>%
  mutate(
    max_power_percent = max_power_load / estimated_1rm * 100,
    max_power_velocity = predict_velocity_at_max_power(
      filtered_data_with_count %>% filter(name == .data$name & squat_type == .data$squat_type),
      max_power_load
    )
  )

# Summarize calculated metrics in a distinct table
summary_table <- cleaned_combined_data %>%
  distinct(name, squat_type, estimated_1rm, max_power_load, max_power_percent, max_power_velocity) %>%
  arrange(name, squat_type)

# Calculate the number of squats per athlete for standard deviation calculation
squats_per_athlete_detail <- cleaned_combined_data %>%
  group_by(squat_type, name) %>%
  summarize(squat_count = n(), .groups = 'drop')

# Add squat count to summary table for further analysis
summary_table <- summary_table %>%
  left_join(squats_per_athlete_detail, by = c("squat_type", "name"))

# Create results table with averages and standard deviations of main metrics, descriptions of squat types
results_table <- cleaned_combined_data %>%
  group_by(squat_type) %>%
  summarise(
    athlete_count = n_distinct(name),
    squat_count = n(),
    avg_squats_per_athlete = round(squat_count / athlete_count),
    squats_per_athlete_sd = round(sd(squats_per_athlete_detail$squat_count[squats_per_athlete_detail$squat_type == unique(squat_type)]), 1),
    avg_estimated_1rm = round(mean(estimated_1rm)),
    estimated_1rm_sd = round(sd(estimated_1rm)),
    avg_max_power_load = round(mean(max_power_load)),
    max_power_load_sd = round(sd(max_power_load)),
    avg_max_power_percent = round(mean(max_power_percent)),
    max_power_percent_sd = round(sd(max_power_percent), 1),
    avg_max_power_velo = round(mean(max_power_velocity), 2),
    max_power_velo_sd = round(sd(max_power_velocity), 2)
  )

# Export the data
write.csv(results_table, "Squat_power_Results.csv", row.names = FALSE)
