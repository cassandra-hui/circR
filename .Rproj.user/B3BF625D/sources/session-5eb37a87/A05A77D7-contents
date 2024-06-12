#functions

# Define the function to find onset time based on sustained activity above the mean for a given duration
find_onset_time <- function(df, cog_time, daily_mean, onset_roll = 10, sustained_minutes = 30) {
  duration_threshold <- sustained_minutes * 60

  # Onset: first time activity reaches above the mean and stays above for sustained duration before CoG
  df_before_cog <- df %>%
    filter(timestamp <= cog_time) %>%
    arrange(timestamp)

  if (nrow(df_before_cog) == 0) {
    return(NA)
  }

  cumulative_time_above_mean <- 0
  onset_time <- NA
  for (i in seq_len(nrow(df_before_cog))) {
    if (!is.na(df_before_cog$rolling_mean_onset[i]) && df_before_cog$rolling_mean_onset[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + onset_roll * 60
      if (cumulative_time_above_mean >= duration_threshold) {
        # Check if the activity falls back below the mean
        if (all(df_before_cog$rolling_mean_onset[i:(i + duration_threshold / (onset_roll * 60))] > daily_mean, na.rm = TRUE)) {
          onset_time <- df_before_cog$timestamp[i]
          break
        }
      }
    } else {
      cumulative_time_above_mean <- 0
    }
  }
  return(onset_time)
}

# Calculate daily mean, onset time with custom function, and offset time with rolling mean of 30
calculate_metrics <- function(data, onset_roll = 10, offset_roll = 30) {
  data <- data %>%
    arrange(timestamp) %>%
    mutate(rolling_mean_onset = rollmean(HopsPerMinute, k = onset_roll, fill = NA, align = "right"),
           rolling_mean_offset = rollmean(HopsPerMinute, k = offset_roll, fill = NA, align = "right"))

  daily_mean <- mean(data$HopsPerMinute, na.rm = TRUE)
  cog_time <- as.POSIXct(data$timestamp[floor(nrow(data)/2)], origin="1970-01-01") # CoG time

  onset_time <- find_onset_time(data, cog_time, daily_mean, onset_roll = onset_roll, sustained_minutes = 30)

  offset_time <- data %>%
    filter(rolling_mean_offset > daily_mean) %>%
    slice_tail(n = 1) %>%
    pull(timestamp)

  if (length(offset_time) == 0) {
    offset_time <- NA
  }

  data <- data %>%
    mutate(daily_mean = daily_mean,
           onset_time = onset_time,
           offset_time = offset_time)

  return(data)
}

activity_times <- function(data, onset_roll = 10, offset_roll = 30){
  # Filter data for all cages
  unique_cages <- unique(data$Cage)

  # Initialize a list to store results
  results <- list()

  # Loop through each cage and date to calculate metrics
  for (cage in unique_cages) {
    cage_data <- data %>%
      filter(Cage == cage)

    unique_dates <- unique(cage_data$Date)

    for (date in unique_dates) {
      daily_data <- cage_data %>%
        filter(Date == date)

      if (nrow(daily_data) > 0) {
        metrics <- calculate_metrics(daily_data, onset_roll = onset_roll, offset_roll = offset_roll)
        results[[paste(cage, date, sep = "_")]] <- metrics
      }
    }
  }

  # Combine results into a single data frame
  combined_data <- bind_rows(results)



}

# # Filter data for all cages
# unique_cages <- unique(df$Cage)
#
# # Initialize a list to store results
# results <- list()
#
# # Loop through each cage and date to calculate metrics
# for (cage in unique_cages) {
#   cage_data <- df %>%
#     filter(Cage == cage)
#
#   unique_dates <- unique(cage_data$Date)
#
#   for (date in unique_dates) {
#     daily_data <- cage_data %>%
#       filter(Date == date)
#
#     if (nrow(daily_data) > 0) {
#       metrics <- calculate_metrics(daily_data, onset_roll = onset_roll)
#       results[[paste(cage, date, sep = "_")]] <- metrics
#     }
#   }
# }
#
# # Combine results into a single data frame
# combined_data <- bind_rows(results)

# Function to extract and print onset and offset times for a specific cage
extract_times <- function(data, cage_number) {
  on_off_times <- data %>%
    filter(Cage == cage_number) %>%
    select(Cage, Date, onset_time, offset_time) %>%
    distinct() %>%
    arrange(Date)

  print(on_off_times)
}

# Example: Extract and print onset and offset times for Cage 20
#extract_times(20)

# Function to plot data for a specific cage
plot_data <- function(data, cage_number) {
  cage_data <- data %>%
    filter(Cage == cage_number)

  ggplot(cage_data, aes(x = timestamp, y = rolling_mean_onset)) +
    geom_rect(data = cage_data,
              aes(xmin = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M"),
                  xmax = as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p"), ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_rect(data = cage_data,
              aes(xmin = as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p"),
                  xmax = as.POSIXct(paste(Date, "23:59"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_line(aes(y = rolling_mean_onset), color = "black") +
    geom_hline(data = cage_data %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
    geom_point(aes(x = onset_time, y = 0), color = "blue", size = 2) +
    geom_point(aes(x = offset_time, y = 0), color = "blue", size = 2) +
    facet_wrap(~ Date, scales = "free_x", ncol = 1) +
    labs(title = paste("Rolling Average and Onset/Offset Relative to CoG for Cage", cage_number), x = "Time", y = "Rolling Average") +
    theme_classic()
}

# Example: Plot data for Cage 20
#plot_cage_data(20)


#Example Usage
com_dat <- activity_times(df, onset_roll = 10, offset_roll = 10)
extract_times(com_dat, 2)
plot_data(com_dat, 2)
