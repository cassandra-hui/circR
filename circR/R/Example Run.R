#Example Run

library(dplyr)
library(zoo)
library(ggplot2)

# Assuming 'df' is your data frame with the required columns
onset_roll <- 10
offset_roll <- 30


com_data <- activity_times(df, onset_roll = onset_roll, offset_roll = offset_roll)

# Extract and print onset and offset times for Cage 20
extract_times(com_data, 20)

# Plot data for Cage 20
plot_cage_data(com_data, 20)
