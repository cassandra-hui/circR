find_offset <- function(data) {
  data <- data %>%
    arrange(timestamp) %>%
    mutate(rolling_mean_30 = rollmean(HopsPerMinute, k = 30, fill = NA, align = "right"))

  daily_mean <- mean(data$HopsPerMinute, na.rm = TRUE)
  cog_time <- as.POSIXct(data$timestamp[floor(nrow(data) / 2)], origin="1970-01-01") # Example CoG time

  offset_time <- data %>%
    filter(rolling_mean_30 > daily_mean) %>%
    slice_tail(n = 1) %>%
    pull(timestamp)

  if (length(offset_time) == 0) {
    offset_time <- NA
  }

  data <- data %>%
    mutate(daily_mean = daily_mean,
           offset_time = offset_time)

  return(data)
}
