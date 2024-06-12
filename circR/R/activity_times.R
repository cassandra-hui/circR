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
