#' Calculate Activity Onset and Offset Times for Multiple Cages
#'
#' This function calculates the activity onset and offset times for multiple cages over different dates.
#'
#' @param data A data frame containing activity data with columns 'Cage', 'Date', 'HopsPerMinute', and 'timestamp'.
#' @param onset_roll An integer specifying the window size for the rolling mean used to calculate onset times. Default is 10.
#' @param offset_roll An integer specifying the window size for the rolling mean used to calculate offset times. Default is 30.
#' @return A data frame containing the combined results with calculated metrics for each cage and date.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr bind_rows
#' @importFrom zoo rollmean
#' @importFrom dplyr %>%
#' @examples
#' # Assuming df is your data frame with the required columns
#' combined_data <- activity_times(df, onset_roll = 10, offset_roll = 30)
#' @export
activity_times <- function(data, onset_roll = 30, offset_roll = 30, sustained_minutes = 30){
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
        metrics <- calculate_metrics(daily_data, onset_roll = onset_roll, offset_roll = offset_roll, sustained_minutes = sustained_minutes)
        results[[paste(cage, date, sep = "_")]] <- metrics
      }
    }
  }

  # Combine results into a single data frame
  combined_data <- bind_rows(results)

}
