# Install and load appropriate packages
install.packages("tidyverse")
install.packages("gridExtra")
packages <- c("dplyr", "ggplot2", "tidyr", "lubridate", "readr", "scales", "gridExtra")
lapply(packages, library, character.only = TRUE)

# Load and format data
data <- read_csv("BSB_Force_Plate_Raw.csv") %>%
  mutate(date = as.Date(date))

# Remove outliers based on jump height and MRSI per athlete
cleaned_data <- data %>%
  group_by(name) %>%
  mutate(
    is_outlier_jump = abs(jump_height - mean(jump_height, na.rm = TRUE)) > 3 * sd(jump_height, na.rm = TRUE),
    is_outlier_mrsi = abs(mrsi - mean(mrsi, na.rm = TRUE)) > 3 * sd(mrsi, na.rm = TRUE)
  ) %>%
  filter(!is_outlier_jump, !is_outlier_mrsi) %>%
  ungroup() %>%
  select(-is_outlier_jump, -is_outlier_mrsi)

# Organize data by month and calculate monthly averages by position
monthly_avg <- cleaned_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month, position) %>%
  summarise(
    avg_jump_height = round(mean(jump_height, na.rm = TRUE), 2),
    sd_jump_height = round(sd(jump_height, na.rm = TRUE), 2),
    jump_count = n(),
    avg_mrsi = round(mean(mrsi, na.rm = TRUE), 2),
    sd_mrsi = round(sd(mrsi, na.rm = TRUE), 2),
    mrsi_count = n(),
    .groups = 'drop'
  )

# Create summary tables for jump height
summary_jump_table <- monthly_avg %>%
  select(month, position, avg_jump_height, sd_jump_height, jump_count) %>%
  mutate(month = factor(format(month, "%B"), 
                        levels = unique(format(month, "%B")))) %>%
  arrange(position, month)

# Create summary table for mRSI
summary_mrsi_table <- monthly_avg %>%
  select(month, position, avg_mrsi, sd_mrsi, mrsi_count) %>%
  mutate(month = factor(format(month, "%B"), 
                        levels = unique(format(month, "%B")))) %>%
  arrange(position, month)

# Join tables for export
combined_summary_table <- left_join(
  summary_jump_table,
  summary_mrsi_table,
  by = c("month", "position")
)

# Plot for Jump Height
jump_plot <- ggplot(monthly_avg, aes(x = month, y = avg_jump_height, color = position)) +
  geom_line() + geom_point(size = 2) +
  labs(title = "Average Jump Height by Position by Month", x = "Month", y = "Average Jump Height (m)") +
  theme_minimal() +
  scale_x_date(date_labels = "%b", breaks = seq(min(monthly_avg$month), max(monthly_avg$month), by = "1 month")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ position)

print(jump_plot)

# Plot for mRSI
mrsi_plot <- ggplot(monthly_avg, aes(x = month, y = avg_mrsi, color = position)) +
  geom_line() + geom_point(size = 2) +
  labs(title = "Average MRSI by Position by Month", x = "Month", y = "Average MRSI") +
  theme_minimal() +
  scale_x_date(date_labels = "%b", breaks = seq(min(monthly_avg$month), max(monthly_avg$month), by = "1 month")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ position)

print(mrsi_plot)