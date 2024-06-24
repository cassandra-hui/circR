#' Find Onset Time
#'
#' This function calculates the onset time based on sustained activity above the mean for a given duration.
#'
#' @param df A data frame containing the activity data with columns 'timestamp', 'HopsPerMinute', and 'rolling_mean_onset'.
#' @param cog_time The center of gravity time as a POSIXct object.
#' @param daily_mean The daily mean activity level.
#' @param onset_roll The rolling mean window for onset calculation, default is 10 minutes.
#' @param sustained_minutes The number of minutes the activity needs to stay above the mean to be considered as sustained, default is 30 minutes.
#'
#' @return A POSIXct object representing the onset time if found, otherwise NA.
#'
#' @import dplyr
#' @import zoo
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr bind_rows
#' @importFrom zoo rollmean
#' @importFrom dplyr %>%
#'
#'
#' @export
#'
#' @examples
#' # Example usage:
#' df <- data.frame(
#'   timestamp = seq.POSIXt(from = as.POSIXct("2023-03-01 00:00:00"),
#'                          to = as.POSIXct("2023-03-01 23:59:00"), by = "min"),
#'   HopsPerMinute = rpois(1440, lambda = 2)
#' )
#' df <- df %>%
#'   arrange(timestamp) %>%
#'   mutate(rolling_mean_onset = zoo::rollmean(HopsPerMinute, k = 10, fill = NA, align = "right"))
#'
#' daily_mean <- mean(df$HopsPerMinute, na.rm = TRUE)
#' cog_time <- as.POSIXct(df$timestamp[floor(nrow(df) / 2)], origin = "1970-01-01")
#'
#' onset_time <- find_onset_time(df, cog_time, daily_mean, onset_roll = 10, sustained_minutes = 30)
#' print(onset_time)

find_onset_time <- function(df, cog_time, daily_mean, onset_roll, sustained_minutes) {
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
