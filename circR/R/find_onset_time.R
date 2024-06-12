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
