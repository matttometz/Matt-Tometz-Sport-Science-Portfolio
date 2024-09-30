#Install tidyverse and load packages
install.packages("tidyverse")
library(dplyr)
library(tidyr)
library(lubridate)

#Load data and re-format as necessary
data <- read.csv("LVP_1080.csv")
data$date <- as.Date(data$date)

#Reshape data from wide to long
data_long <- pivot_longer(data,
                          cols = c(load_1, load_2, load_3, load_4,
                                   velo_1, velo_2, velo_3, velo_4,
                                   power_1, power_2, power_3, power_4),
                          names_to = c(".value", "set"),
                          names_pattern = "(load|velo|power)_(\\d+)")

# Calculate velocity decrements of profiles per athlete
calc_velocity_decrements <- function(df) {
  model <- lm(velo ~ load, data = df)
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  max_velocity <- intercept
  load_90 <- (max_velocity * 0.90 - intercept) / slope
  load_75 <- (max_velocity * 0.75 - intercept) / slope
  load_50 <- (max_velocity * 0.50 - intercept) / slope
  
  return(data.frame(
    max_velocity = max_velocity, 
    load_90 = load_90, 
    load_75 = load_75, 
    load_50 = load_50,
    r_squared = summary(model)$r.squared
  ))
}

#Run function for velocity decrements
results <- data_long %>%
  group_by(Name, gender, age) %>%
  do(calc_velocity_decrements(.)) %>%
  ungroup()

# Add the date back for filtering later
results <- results %>%
  left_join(data_long %>% select(Name, gender, age, date), by = c("Name", "gender", "age"))

# Filter out error entries for gender or aren't in desired age groups
filtered_data <- data_long %>%
  filter(gender %in% c(1, 2) & age %in% c(1, 2, 3))

#Filter most recent profiles for each athlete
most_recent_profiles <- results %>%
  group_by(Name, gender, age) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()

#Sort groups based on age and gender
most_recent_profiles <- most_recent_profiles %>%
  mutate(Group = case_when(
    age == 1 & gender == 1 ~ "High School Boys",
    age == 2 & gender == 1 ~ "College Boys",
    age == 3 & gender == 1 ~ "Pro Boys",
    age == 1 & gender == 2 ~ "High School Girls",
    age == 2 & gender == 2 ~ "College Girls",
    age == 3 & gender == 2 ~ "Pro Girls"
  ))

#Calculate averages and standard deviations for each group
averages <- most_recent_profiles %>%
  group_by(Group) %>%
  summarise(
    number_of_profiles = n(),
    avg_max_velocity = round(mean(max_velocity, na.rm = TRUE), 2),
    sd_max_velocity = round(sd(max_velocity, na.rm = TRUE), 2),
    avg_load_90 = round(mean(load_90, na.rm = TRUE), 1),
    sd_load_90 = round(sd(load_90, na.rm = TRUE), 1),
    avg_load_75 = round(mean(load_75, na.rm = TRUE), 1),
    sd_load_75 = round(sd(load_75, na.rm = TRUE), 1),
    avg_load_50 = round(mean(load_50, na.rm = TRUE), 1),
    sd_load_50 = round(sd(load_50, na.rm = TRUE), 1),
    avg_r_squared = round(mean(r_squared, na.rm = TRUE), 2),  # Average R-squared
    sd_r_squared = round(sd(r_squared, na.rm = TRUE), 2)      # SD of R-squared
  ) %>%
  filter(!is.na(Group))

#Renaming columns for easier readability
averages <- averages %>%
  rename(
    "Athletes" = number_of_profiles,
    "Avg Max Velo" = avg_max_velocity,
    "Max Velo SD" = sd_max_velocity,
    "10% Vdec" = avg_load_90,
    "10% SD" = sd_load_90,
    "25% Vdec" = avg_load_75,
    "25% SD" = sd_load_75,
    "50% Vdec" = avg_load_50,
    "50% SD" = sd_load_50,
    "Avg R2" = avg_r_squared,
    "R2 SD" = sd_r_squared
  )

#Export data
write.csv(averages, "LVP_group_averages.csv", row.names = FALSE)
