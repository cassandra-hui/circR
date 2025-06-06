#Script

#########################

library(Ouyang)


setwd("~/UNR/Social/ALAN/Data/Behavior")

# Specify the directory containing the data files
data_dir <- "~/UNR/Social/ALAN/Data/Behavior/Raw Data"


# Get a list of all the data files in the directory
file_paths <- list.files(data_dir, full.names = TRUE)

# Reformat data files
data <- reformat_data_files(file_paths)

# Attach metadata and calculate phase
data <- attach_meta(data, "meta.csv")

# Print the final data frame with metadata and phase
head(data)

################################

#Set working directory
setwd("~/GitHub/circR/Test")

#Rename data
df <- data


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
