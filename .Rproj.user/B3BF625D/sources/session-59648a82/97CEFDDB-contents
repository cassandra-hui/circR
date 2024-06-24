#' Calculate Daily Activity Metrics
#'
#' This function calculates the daily activity metrics, including rolling means for onset and offset times,
#' daily mean activity, and the onset and offset times based on sustained activity.
#'
#' @param data A data frame containing activity data with columns 'Cage', 'Date', 'HopsPerMinute', and 'timestamp'.
#' @param onset_roll An integer specifying the window size for the rolling mean used to calculate onset times. Default is 10.
#' @param offset_roll An integer specifying the window size for the rolling mean used to calculate offset times. Default is 30.
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
#' @return A data frame with additional columns for daily mean activity, rolling means for onset and offset,
#'         and the calculated onset and offset times.
#' @examples
#' # Assuming df is your data frame with the required columns
#' metrics_data <- calculate_metrics(df, onset_roll = 10, offset_roll = 30)
#' @export
calculate_metrics <- function(data, onset_roll = 10, offset_roll = 30, sustained_minutes = 30) {
  data <- data %>%
    arrange(timestamp) %>%
    mutate(rolling_mean_onset = rollmean(HopsPerMinute, k = onset_roll, fill = NA, align = "right"),
           rolling_mean_offset = rollmean(HopsPerMinute, k = offset_roll, fill = NA, align = "right"))

  daily_mean <- mean(data$HopsPerMinute, na.rm = TRUE)
  cog_time <- as.POSIXct(data$timestamp[floor(nrow(data)/2)], origin="1970-01-01") # CoG time

  onset_time <- find_onset_time(data, cog_time, daily_mean, onset_roll = onset_roll, sustained_minutes = sustained_minutes)

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
