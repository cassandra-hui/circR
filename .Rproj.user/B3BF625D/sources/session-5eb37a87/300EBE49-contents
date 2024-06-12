# Function to extract and print onset and offset times for a specific cage
extract_times <- function(data, cage_number) {
  on_off_times <- data %>%
    filter(Cage == cage_number) %>%
    select(Cage, Date, onset_time, offset_time) %>%
    distinct() %>%
    arrange(Date)

  print(on_off_times)
}
