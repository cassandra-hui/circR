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
