##########################

library(Ouyang)


setwd("~/UNR/Social/ALAN/Data/Behavior")

# Specify the directory containing the data files
data_dir <- "~/UNR/Social/ALAN/Data/Behavior/Raw Data"
#data_dir <- "~/UNR/Social/ALAN/Data/Behavior/Raw Data2"
#data_dir <- "~/UNR/Social/ALAN/Data/Behavior/Raw Data3"
#data_dir <- "~/UNR/Social/ALAN/Data/Behavior/Raw Data4"

# Get a list of all the data files in the directory
file_paths <- list.files(data_dir, full.names = TRUE)

# Reformat data files
data <- reformat_data_files(file_paths)

# Attach metadata and calculate phase
data <- attach_meta(data, "meta.csv")
#data <- attach_meta(data, "meta2.csv")
#data <- attach_meta(data, "meta3.csv")
#data <- attach_meta(data, "meta4.csv")

# Print the final data frame with metadata and phase
head(data)

################################




setwd("~/GitHub/circR/circR/Test")

df <- data

#review
summary(df)
str(df)

head(df)



#Install Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)


#Already have minute data

# # Arrange and group data, then summarize by the minute
# df_clean <- df %>%
#   arrange(ID) %>%
#   mutate(datetime = floor_date(datetime, "minute"),  # Round `datetime` to minute precision
#          datetime_str = format(datetime, "%Y-%m-%d %H:%M")) %>%  # Format to remove seconds
#   group_by(date, ID, datetime) %>%             # Group by formatted datetime
#   summarise(activity = sum(activity), .groups = "drop")  # Summarize activity
#
#
# df_clean$datetime <- as.POSIXct(df_clean$datetime_str, format = "%Y-%m-%d %H:%M")
# df_clean$datetime_str <- NULL



df_clean <- df


#Split birds into their own data frame
unique_ids <- unique(df_clean$Cage)

for (id in unique_ids) {
  df_name <- paste("Cage", id, sep = "_")
  assign(df_name, subset(df_clean, Cage == id))
}

#looks good


######
# Now all the birds are in different data frames
######

#Let's test one
# Plot
ggplot(Cage_15, aes(x = timestamp, y = HopsPerMinute)) +
  geom_line() +
  labs(title = "Activity Over Time",
       x = "Time",
       y = "Activity") +
  theme_minimal()


############################################
# Cut to one date                          #
############################################


# Here is an example for bird 15, you can do this manually or create a loop or however you like

# Sort
Cage_15 <- Cage_15 %>% arrange(timestamp)

#Cut
Cage_15_1 <-subset(Cage_15, Date == "2023-03-03")


# Plot to see activity
ggplot(Cage_15_1, aes(x = timestamp, y = HopsPerMinute)) +
  geom_line() +
  labs(title = "Activity Over Time",
       x = "Time",
       y = "Activity") +
  theme_minimal()


#Create rolling means
roll_m15<-Cage_15_1 %>%
  mutate(rolling_avg = data.table::frollmean(HopsPerMinute,21),index=seq(1,nrow(Cage_15_1),1))
roll_m15$rolling_avg[is.na(roll_m15$rolling_avg)] <- 0

#Create daily mean for individual bird
daily_mean_15 <- Cage_15_1 %>%
  group_by(Date) %>%
  summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE))


# Plot rolling mean to see if it looks right
ggplot(roll_m15, aes(x = timestamp, y = rolling_avg)) +
  geom_line() +
  #geom_hline(yintercept = 3.1, color = "red") +  #daily mean for May 22nd
  labs(title = "Rolling mean over days",
       x = "Time",
       y = "Rolling mean (20 Minutes)",
       color = "Dataset") +
  theme_minimal()


# Set threshold for on- and offset marker
# I did not separate by day (most of the ones I saw had similar means over the days)
# You can break this down by individual day
threshold <- mean(daily_mean_15$mean_activity)

# Placeholder for crossing data
crossings_data <- data.frame(timestamp = as.POSIXct(character()), value = numeric())

# Loop to calculate interpolated crossing points
for (i in seq_along(roll_m15$rolling_avg)[-1]) {
  if ((roll_m15$rolling_avg[i-1] < threshold && roll_m15$rolling_avg[i] > threshold) ||
      (roll_m15$rolling_avg[i-1] > threshold && roll_m15$rolling_avg[i] < threshold)) {
    # Linear interpolation for more precise timestamp
    time_diff <- diff(as.numeric(roll_m15$timestamp[(i-1):i]))
    value_diff <- diff(roll_m15$rolling_avg[(i-1):i])
    proportion <- abs(threshold - roll_m15$rolling_avg[i-1]) / value_diff
    exact_time <- as.POSIXct(roll_m15$timestamp[i-1] + time_diff * proportion, origin = "1970-01-01")

    # Store the crossing point
    crossings_data <- rbind(crossings_data, data.frame(timestamp = exact_time, value = threshold))
  }
}

# Ensure datetime format for plotting
crossings_data$datetime <- as.POSIXct(crossings_data$timestamp, origin = "1970-01-01")

#Plot to see if point are accuarate and which ones to ignore
ggplot(roll_m15, aes(x = timestamp, y = rolling_avg)) +
  geom_line(color = "black") +  # Plot the rolling average
  geom_hline(yintercept = threshold, color = "red", linetype = "dashed") +  # Plot the threshold line
  geom_point(data = crossings_data, aes(x = datetime, y = value), color = "blue", size = 3, shape = 19) +  # Plot crossing points
  labs(title = "Rolling Average and Interpolated Crossings",
       x = "Time",
       y = "Rolling Average") +
  theme_minimal()

#Here is your data (select only one around day activity)
print(crossings_data)

##########################

# Here is an example for bird 15, you can do this manually or create a loop or however you like

# Sort
Cage_15 <- Cage_15 %>% arrange(timestamp)


# Plot to see activity
ggplot(Cage_15, aes(x = timestamp, y = HopsPerMinute)) +
  geom_line() +
  labs(title = "Activity Over Time",
       x = "Time",
       y = "Activity") +
  theme_minimal()


#Create rolling means
roll_m15<-Cage_15 %>%
  mutate(rolling_avg = data.table::frollmean(HopsPerMinute,21),index=seq(1,nrow(Cage_15),1))
roll_m15$rolling_avg[is.na(roll_m15$rolling_avg)] <- 0

#Create daily mean for individual bird
daily_mean_15 <- Cage_15 %>%
  group_by(Date) %>%
  summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE))


# Plot rolling mean to see if it looks right
ggplot(roll_m15, aes(x = timestamp, y = rolling_avg)) +
  geom_line() +
  #geom_hline(yintercept = 3.1, color = "red") +  #daily mean for May 22nd
  labs(title = "Rolling mean over days",
       x = "Time",
       y = "Rolling mean (20 Minutes)",
       color = "Dataset") +
  theme_minimal()


# Set threshold for on- and offset marker
# I did not separate by day (most of the ones I saw had similar means over the days)
# You can break this down by individual day
threshold <- mean(daily_mean_15$mean_activity)

# Placeholder for crossing data
crossings_data <- data.frame(timestamp = as.POSIXct(character()), value = numeric())

# Loop to calculate interpolated crossing points
for (i in seq_along(roll_m15$rolling_avg)[-1]) {
  if ((roll_m15$rolling_avg[i-1] < threshold && roll_m15$rolling_avg[i] > threshold) ||
      (roll_m15$rolling_avg[i-1] > threshold && roll_m15$rolling_avg[i] < threshold)) {
    # Linear interpolation for more precise timestamp
    time_diff <- diff(as.numeric(roll_m15$timestamp[(i-1):i]))
    value_diff <- diff(roll_m15$rolling_avg[(i-1):i])
    proportion <- abs(threshold - roll_m15$rolling_avg[i-1]) / value_diff
    exact_time <- as.POSIXct(roll_m15$timestamp[i-1] + time_diff * proportion, origin = "1970-01-01")

    # Store the crossing point
    crossings_data <- rbind(crossings_data, data.frame(timestamp = exact_time, value = threshold))
  }
}

# Ensure datetime format for plotting
crossings_data$datetime <- as.POSIXct(crossings_data$timestamp, origin = "1970-01-01")

#Plot to see if point are accuarate and which ones to ignore
ggplot(roll_m15, aes(x = timestamp, y = rolling_avg)) +
  geom_line(color = "black") +  # Plot the rolling average
  geom_hline(yintercept = threshold, color = "red", linetype = "dashed") +  # Plot the threshold line
  geom_point(data = crossings_data, aes(x = datetime, y = value), color = "blue", size = 3, shape = 19) +  # Plot crossing points
  labs(title = "Rolling Average and Interpolated Crossings",
       x = "Time",
       y = "Rolling Average") +
  theme_minimal()

#Here is your data (select only one around day activity)
print(crossings_data)


######################

library(zoo)
library(ggplot2)
library(dplyr)

# Assuming df is your data frame with columns: timestamp, activity

# Calculate the 20-minute rolling mean
df1 <- Cage_15 %>%
  mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))


df1<-Cage_15 %>%
  mutate(rolling_mean = data.table::frollmean(HopsPerMinute,21),index=seq(1,nrow(Cage_15),1))
df1$rolling_mean[is.na(df1$rolling_mean)] <- 0


# Calculate the daily mean activity
daily_mean <- mean(df1$HopsPerMinute, na.rm = TRUE)

# Find the intersection points
df1 <- df1 %>%
  mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                              (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))



# # Plot the results
# ggplot(df1, aes(x = timestamp, y = rolling_mean)) +
#   geom_line() +
#   geom_hline(yintercept = daily_mean, linetype = "dashed", color = "red") +
#   geom_point(data = df1 %>% filter(intersect == TRUE), aes(y = rolling_mean), color = "blue", size = 3) +
#   labs(title = "Rolling Average and Interpolated Crossings", x = "Time", y = "Rolling Average") +
#   theme_classic()
#



# Identify the periods of sustained activity
df1 <- df1 %>%
  mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))

# Filter for meaningful onsets and offsets
activity_periods <- df1 %>%
  group_by(activity_period) %>%
  filter(intersect == TRUE) %>%
  summarise(onset = min(timestamp), offset = max(timestamp)) %>%
  ungroup()

# Merge the activity periods back to the original dataframe
df1 <- df1 %>%
  left_join(activity_periods, by = "activity_period") %>%
  filter(!is.na(onset))

# Plot the results
ggplot(df1, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(yintercept = daily_mean, linetype = "dashed", color = "red") +
  geom_point(data = df1 %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
  labs(title = "Rolling Average and Interpolated Crossings", x = "Time", y = "Rolling Average") +
  theme_classic()

##




# Calculate the 20-minute rolling mean
df1 <- df1 %>%
  group_by(Date = as.Date(timestamp)) %>%
  mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center")) %>%
  ungroup()

# Fill NA values with 0 for rolling_mean
df1$rolling_mean[is.na(df1$rolling_mean)] <- 0

# Calculate the daily mean activity for each day
daily_means <- df1 %>%
  group_by(Date = as.Date(timestamp)) %>%
  summarise(daily_mean = mean(HopsPerMinute, na.rm = TRUE))

# Merge daily means back to the main data frame
df1 <- df1 %>%
  left_join(daily_means, by = c("Date" = "Date"))

# Find the intersection points
df1 <- df1 %>%
  mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                              (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

# Ensure the 'intersect' column is logical (TRUE/FALSE)
df1$intersect <- as.logical(df1$intersect)

# Identify the periods of sustained activity
df1 <- df1 %>%
  group_by(Date) %>%
  mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0))) %>%
  ungroup()

# Filter for meaningful onsets and offsets
activity_periods <- df1 %>%
  group_by(Date, activity_period) %>%
  filter(intersect == TRUE) %>%
  summarise(onset = min(timestamp), offset = max(timestamp)) %>%
  ungroup()

# Merge the activity periods back to the original dataframe
df1 <- df1 %>%
  left_join(activity_periods, by = c("Date" = "Date", "activity_period" = "activity_period")) %>%
  filter(!is.na(onset))

# Plot the results
ggplot(df1, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = daily_means, aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_point(data = df1 %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
  labs(title = "Rolling Average and Interpolated Crossings", x = "Time", y = "Rolling Average") +
  theme_classic()


######


library(zoo)
library(ggplot2)
library(dplyr)

# Assuming df1 is your data frame with columns: timestamp, HopsPerMinute

# Convert timestamp to Date to group by day
df1 <- Cage_15 %>%
  mutate(Date = as.Date(timestamp))

# Initialize an empty list to store results for each day
results <- list()

# Loop through each unique date
for (date in unique(df1$Date)) {
  # Filter data for the current date
  df_day <- df1 %>%
    filter(Date == date)

  # Calculate the 20-minute rolling mean
  df_day <- df_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))

  # Fill NA values with 0 for rolling_mean
  df_day$rolling_mean[is.na(df_day$rolling_mean)] <- 0

  # Calculate the daily mean activity
  daily_mean <- mean(df_day$HopsPerMinute, na.rm = TRUE)

  # Find the intersection points
  df_day <- df_day %>%
    mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                                (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

  # Ensure the 'intersect' column is logical (TRUE/FALSE)
  df_day$intersect <- as.logical(df_day$intersect)

  # Identify the periods of sustained activity
  df_day <- df_day %>%
    mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))

  # Filter for meaningful onsets and offsets
  activity_periods <- df_day %>%
    group_by(activity_period) %>%
    filter(intersect == TRUE) %>%
    summarise(onset = min(timestamp), offset = max(timestamp)) %>%
    ungroup()

  # Merge the activity periods back to the original dataframe
  df_day <- df_day %>%
    left_join(activity_periods, by = "activity_period") %>%
    filter(!is.na(onset))

  # Store the results for the current day
  results[[as.character(date)]] <- df_day
}

# Combine results into a single data frame
df_combined <- bind_rows(results)

# Plot the results
ggplot(df_combined, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_point(data = df_combined %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
  labs(title = "Rolling Average and Interpolated Crossings", x = "Time", y = "Rolling Average") +
  theme_classic()

############

library(zoo)
library(ggplot2)
library(dplyr)

# Assuming df1 is your data frame with columns: timestamp, HopsPerMinute

# Convert timestamp to Date to group by day
df1 <- Cage_15 %>%
  mutate(Date = as.Date(timestamp))



# Initialize an empty list to store results for each day
results <- list()

# Loop through each unique date
for (date in unique(df1$Date)) {
  # Filter data for the current date
  df_day <- df1 %>%
    filter(Date == date)

  # Calculate the 20-minute rolling mean
  df_day <- df_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))

  # Fill NA values with 0 for rolling_mean
  df_day$rolling_mean[is.na(df_day$rolling_mean)] <- 0

  # Calculate the daily mean activity
  daily_mean <- mean(df_day$HopsPerMinute, na.rm = TRUE)
  df_day$daily_mean <- daily_mean

  # Find the intersection points
  df_day <- df_day %>%
    mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                                (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

  # Ensure the 'intersect' column is logical (TRUE/FALSE)
  df_day$intersect <- as.logical(df_day$intersect)

  # Identify the periods of sustained activity
  df_day <- df_day %>%
    mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))

  # Filter for meaningful onsets and offsets
  activity_periods <- df_day %>%
    group_by(activity_period) %>%
    filter(intersect == TRUE) %>%
    summarise(onset = min(timestamp), offset = max(timestamp)) %>%
    ungroup()

  # Merge the activity periods back to the original dataframe
  df_day <- df_day %>%
    left_join(activity_periods, by = "activity_period") %>%
    filter(!is.na(onset))

  # Store the results for the current day
  results[[as.character(date)]] <- df_day
}

# Combine results into a single data frame
df_combined <- bind_rows(results)

# Plot the results
ggplot(df_combined, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_combined %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_point(data = df_combined %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
  labs(title = "Rolling Average and Interpolated Crossings", x = "Time", y = "Rolling Average") +
  theme_classic()


############

library(zoo)
library(ggplot2)
library(dplyr)

# Assuming df1 is your data frame with columns: timestamp, HopsPerMinute

# Convert timestamp to Date to group by day
df1 <- Cage_15 %>%
  mutate(Date = as.Date(timestamp))

# Initialize an empty list to store results for each day
results <- list()

# Loop through each unique date
for (date in unique(df1$Date)) {
  # Filter data for the current date
  df_day <- df1 %>%
    filter(Date == date)

  # Calculate the 20-minute rolling mean
  df_day <- df_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))

  # Fill NA values with 0 for rolling_mean
  df_day$rolling_mean[is.na(df_day$rolling_mean)] <- 0

  # Calculate the daily mean activity
  daily_mean <- mean(df_day$HopsPerMinute, na.rm = TRUE)
  df_day$daily_mean <- daily_mean

  # Find the intersection points
  df_day <- df_day %>%
    mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                                (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

  # Ensure the 'intersect' column is logical (TRUE/FALSE)
  df_day$intersect <- as.logical(df_day$intersect)

  # Identify the periods of sustained activity
  df_day <- df_day %>%
    mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))

  # Filter for meaningful onsets and offsets
  activity_periods <- df_day %>%
    group_by(activity_period) %>%
    filter(intersect == TRUE) %>%
    summarise(onset = min(timestamp), offset = max(timestamp)) %>%
    ungroup()

  # Merge the activity periods back to the original dataframe
  df_day <- df_day %>%
    left_join(activity_periods, by = "activity_period") %>%
    filter(!is.na(onset))

  # Store the results for the current day
  results[[as.character(date)]] <- df_day
}

# Combine results into a single data frame
df_combined <- bind_rows(results)

# Plot the results
ggplot(df_combined, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_combined %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_point(data = df_combined %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Interpolated Crossings", x = "Time", y = "Rolling Average") +
  theme_classic()


##########


library(zoo)
library(ggplot2)
library(dplyr)

# Assuming df1 is your data frame with columns: timestamp, HopsPerMinute

# Convert timestamp to Date to group by day
df1 <- Cage_15 %>%
  mutate(Date = as.Date(timestamp))

# Initialize an empty list to store results for each day
results <- list()

# Loop through each unique date
for (date in unique(df1$Date)) {
  # Debug: Print the current date being processed
  print(paste("Processing date:", as.Date(date, origin = "1970-01-01")))

  # Filter data for the current date
  df_day <- df1 %>%
    filter(Date == date)

  # Debug: Print the number of rows for the current day
  print(paste("Number of rows for", as.Date(date, origin = "1970-01-01"), ":", nrow(df_day)))

  # Calculate the 20-minute rolling mean for the current day
  df_day <- df_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))

  # Fill NA values with 0 for rolling_mean
  df_day$rolling_mean[is.na(df_day$rolling_mean)] <- 0

  # Calculate the daily mean activity
  daily_mean <- mean(df_day$HopsPerMinute, na.rm = TRUE)
  df_day$daily_mean <- daily_mean

  # Find the intersection points
  df_day <- df_day %>%
    mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                                (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

  # Ensure the 'intersect' column is logical (TRUE/FALSE)
  df_day$intersect <- as.logical(df_day$intersect)

  # Identify the periods of sustained activity
  df_day <- df_day %>%
    mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))

  # Filter for meaningful onsets and offsets
  activity_periods <- df_day %>%
    group_by(activity_period) %>%
    filter(intersect == TRUE) %>%
    summarise(onset = min(timestamp), offset = max(timestamp)) %>%
    ungroup()

  # Merge the activity periods back to the original dataframe
  df_day <- df_day %>%
    left_join(activity_periods, by = "activity_period") %>%
    filter(!is.na(onset))

  # Store the results for the current day
  results[[as.character(date)]] <- df_day
}

# Combine results into a single data frame
df_combined <- bind_rows(results)

# Debug: Print the first few rows of the combined dataframe
print(head(df_combined))

# Plot the results
ggplot(df_combined, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_combined %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_point(data = df_combined %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Interpolated Crossings", x = "Time", y = "Rolling Average") +
  theme_classic()

###############################


library(zoo)
library(ggplot2)
library(dplyr)

# Assuming df1 is your data frame with columns: timestamp, HopsPerMinute

# Convert timestamp to Date to group by day
df1 <- Cage_15 %>%
  mutate(Date = as.Date(timestamp))

# Isolate data for a single day
single_day <- df1 %>%
  filter(Date == as.Date("2023-03-06"))

# Print the first few rows of the single day's data
print(head(single_day))

# Calculate the 20-minute rolling mean for the single day
single_day <- single_day %>%
  mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))

# Fill NA values with 0 for rolling_mean
single_day$rolling_mean[is.na(single_day$rolling_mean)] <- 0

# Print the first few rows after calculating the rolling mean
print(head(single_day))

# Calculate the daily mean activity
daily_mean <- mean(single_day$HopsPerMinute, na.rm = TRUE)
single_day$daily_mean <- daily_mean

# Find the intersection points
single_day <- single_day %>%
  mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                              (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

# Print the first few rows after finding intersections
print(head(single_day))

# Ensure the 'intersect' column is logical (TRUE/FALSE)
single_day$intersect <- as.logical(single_day$intersect)

# Identify the periods of sustained activity
single_day <- single_day %>%
  mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))

# Filter for meaningful onsets and offsets
activity_periods <- single_day %>%
  group_by(activity_period) %>%
  filter(intersect == TRUE) %>%
  summar


###############

library(zoo)
library(ggplot2)
library(dplyr)

# Assuming df1 is your data frame with columns: timestamp, HopsPerMinute

# Explicitly extract the date from the timestamp
df1 <- Cage_15 %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Print the first few rows to check if Date aligns with timestamp
print(head(df1))

# Isolate data for a single day
single_day <- df1 %>%
  filter(Date == as.Date("2023-03-06"))

# Print the first few rows of the single day's data to verify alignment
print(head(single_day))

# Calculate the 20-minute rolling mean for the single day
single_day <- single_day %>%
  mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))

# Fill NA values with 0 for rolling_mean
single_day$rolling_mean[is.na(single_day$rolling_mean)] <- 0

# Print the first few rows after calculating the rolling mean
print(head(single_day))

# Calculate the daily mean activity
daily_mean <- mean(single_day$HopsPerMinute, na.rm = TRUE)
single_day$daily_mean <- daily_mean

# Find the intersection points
single_day <- single_day %>%
  mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                              (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

# Ensure the 'intersect' column is logical (TRUE/FALSE)
single_day$intersect <- as.logical(single_day$intersect)

# Identify the periods of sustained activity
single_day <- single_day %>%
  mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))

# Filter for meaningful onsets and offsets
activity_periods <- single_day %>%
  group_by(activity_period) %>%
  filter(intersect == TRUE) %>%
  summarise(onset = min(timestamp), offset = max(timestamp)) %>%
  ungroup()

# Merge the activity periods back to the original dataframe
single_day <- single_day %>%
  left_join(activity_periods, by = "activity_period") %>%
  filter(!is.na(onset))

# Print the first few rows after merging activity periods
print(head(single_day))

# Plot the single day's results
ggplot(single_day, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(yintercept = daily_mean, linetype = "dashed", color = "red") +
  geom_point(data = single_day %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
  labs(title = "Single Day Rolling Average", x = "Time", y = "Rolling Average") +
  theme_classic()
##########################

library(zoo)
library(ggplot2)
library(dplyr)

# Assuming df1 is your data frame with columns: timestamp, HopsPerMinute

# Explicitly extract the date from the timestamp
df1 <- Cage_15 %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Initialize an empty list to store results for each day
results <- list()

# Loop through each unique date
for (date in unique(df1$Date)) {
  # Debug: Print the current date being processed
  print(paste("Processing date:", as.Date(date, origin = "1970-01-01")))

  # Filter data for the current date
  df_day <- df1 %>%
    filter(Date == date)

  # Debug: Print the number of rows for the current day
  print(paste("Number of rows for", as.Date(date, origin = "1970-01-01"), ":", nrow(df_day)))

  # Calculate the 20-minute rolling mean for the current day
  df_day <- df_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))

  # Fill NA values with 0 for rolling_mean
  df_day$rolling_mean[is.na(df_day$rolling_mean)] <- 0

  # Calculate the daily mean activity
  daily_mean <- mean(df_day$HopsPerMinute, na.rm = TRUE)
  df_day$daily_mean <- daily_mean

  # Find the intersection points
  df_day <- df_day %>%
    mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                                (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

  # Ensure the 'intersect' column is logical (TRUE/FALSE)
  df_day$intersect <- as.logical(df_day$intersect)

  # Identify the periods of sustained activity
  df_day <- df_day %>%
    mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))

  # Filter for meaningful onsets and offsets
  activity_periods <- df_day %>%
    group_by(activity_period) %>%
    filter(intersect == TRUE) %>%
    summarise(onset = min(timestamp), offset = max(timestamp)) %>%
    ungroup()

  # Merge the activity periods back to the original dataframe
  df_day <- df_day %>%
    left_join(activity_periods, by = "activity_period") %>%
    filter(!is.na(onset))

  # Store the results for the current day
  results[[as.character(date)]] <- df_day
}

# Combine results into a single data frame
df_combined <- bind_rows(results)

# Debug: Print the first few rows of the combined dataframe
print(head(df_combined))

# Plot the results
ggplot(df_combined, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_combined %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_point(data = df_combined %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Interpolated Crossings", x = "Time", y = "Rolling Average") +
  theme_classic()

########################

# Explicitly extract the date from the timestamp
df1 <- df %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Print the structure and first few rows of df1
print(str(df1))
print(head(df1))


# Choose a specific cage for initial debugging
cage <- unique(df1$Cage)[1]  # For example, choose the first Cage
df_cage <- df1 %>%
  filter(Cage == cage)

# Print the structure and first few rows of df_cage
print(str(df_cage))
print(head(df_cage))

# Choose a specific date for initial debugging
date <- unique(df_cage$Date)[1]  # For example, choose the first Date
df_day <- df_cage %>%
  filter(Date == date)

# Print the structure and first few rows of df_day
print(str(df_day))
print(head(df_day))

# Calculate the 20-minute rolling mean for the current day
df_day <- df_day %>%
  mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))

# Fill NA values with 0 for rolling_mean
df_day$rolling_mean[is.na(df_day$rolling_mean)] <- 0

# Print the structure and first few rows after calculating rolling mean
print(str(df_day))
print(head(df_day))

# Calculate the daily mean activity
daily_mean <- mean(df_day$HopsPerMinute, na.rm = TRUE)
df_day$daily_mean <- daily_mean

# Print the daily mean
print(daily_mean)

# Find the intersection points
df_day <- df_day %>%
  mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                              (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

# Ensure the 'intersect' column is logical (TRUE/FALSE)
df_day$intersect <- as.logical(df_day$intersect)

# Print the structure and first few rows after finding intersections
print(str(df_day))
print(head(df_day))


# Identify the periods of sustained activity
df_day <- df_day %>%
  mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))

# Print the structure and first few rows after identifying activity periods
print(str(df_day))
print(head(df_day))

# Filter for meaningful onsets and offsets, ensuring non-empty groups
activity_periods <- df_day %>%
  group_by(activity_period) %>%
  filter(intersect == TRUE) %>%
  summarise(onset = if_else(n() > 0, min(timestamp), as.POSIXct(NA)),
            offset = if_else(n() > 0, max(timestamp), as.POSIXct(NA))) %>%
  ungroup()

# Print the activity periods
print(activity_periods)

# Merge the activity periods back to the original dataframe
df_day <- df_day %>%
  left_join(activity_periods, by = "activity_period") %>%
  filter(!is.na(onset))

# Print the structure and first few rows after merging activity periods
print(str(df_day))
print(head(df_day))


######################

library(zoo)
library(ggplot2)
library(dplyr)

# Explicitly extract the date from the timestamp
df1 <- df %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))


# Initialize an empty list to store results for each Cage
results_all_cages <- list()

# Loop through each unique Cage
for (cage in unique(df1$Cage)) {
  # Filter data for the current Cage
  df_cage <- df1 %>%
    filter(Cage == cage)

  print(cage)

  # Initialize an empty list to store results for each day within the current Cage
  results <- list()

  # Loop through each unique date within the current Cage
  for (date in unique(df_cage$Date)) {
    # Filter data for the current date
    df_day <- df_cage %>%
      filter(Date == date)

    # Calculate the 20-minute rolling mean for the current day
    df_day <- df_day %>%
      mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))

    # Fill NA values with 0 for rolling_mean
    df_day$rolling_mean[is.na(df_day$rolling_mean)] <- 0


    # Calculate the daily mean activity
    daily_mean <- mean(df_day$HopsPerMinute, na.rm = TRUE)
    df_day$daily_mean <- daily_mean


    # Find the intersection points
    df_day <- df_day %>%
      mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                                  (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

    # Ensure the 'intersect' column is logical (TRUE/FALSE)
    df_day$intersect <- as.logical(df_day$intersect)

    # Identify the periods of sustained activity
    df_day <- df_day %>%
      mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))


    # Filter for meaningful onsets and offsets, ensuring non-empty groups
    activity_periods <- df_day %>%
      group_by(activity_period) %>%
      filter(intersect == TRUE) %>%
      summarise(onset = if_else(any(!is.na(timestamp)), min(timestamp, na.rm = TRUE), as.POSIXct(NA)),
                offset = if_else(any(!is.na(timestamp)), max(timestamp, na.rm = TRUE), as.POSIXct(NA))) %>%
      ungroup()


    # Merge the activity periods back to the original dataframe
    df_day <- df_day %>%
      left_join(activity_periods, by = "activity_period") %>%
      filter(!is.na(onset))

    # Store the results for the current day
    results[[as.character(date)]] <- df_day
  }

  # Combine results into a single data frame for the current Cage
  df_combined_cage <- bind_rows(results)

  # Store the combined results for the current Cage in the main list
  results_all_cages[[as.character(cage)]] <- df_combined_cage
}

# Combine results from all Cages into a single data frame
df_combined_all <- bind_rows(results_all_cages)

# Save the results to a file or variable for later use
saveRDS(results_all_cages, file = "results_all_cages.rds")

# Function to plot the results for a specific Cage
plot_cage <- function(cage_number) {
  df_cage_to_plot <- results_all_cages[[as.character(cage_number)]]

  ggplot(df_cage_to_plot, aes(x = timestamp, y = rolling_mean)) +
    geom_line() +
    geom_hline(data = df_cage_to_plot %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
    geom_point(data = df_cage_to_plot %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
    facet_wrap(~ Date, scales = "free_x", ncol = 1) +
    labs(title = paste("Rolling Average and Interpolated Crossings for Cage", cage_number), x = "Time", y = "Rolling Average") +
    theme_classic()
}


plot_cage(20
          )


# Initialize an empty list to store onset and offset times for each cage
onset_offset_times <- list()

# Loop through each cage in the results_all_cages list
for (cage in names(results_all_cages)) {
  df_cage <- results_all_cages[[cage]]

  # Extract the unique onset and offset times
  onset_offset <- df_cage %>%
    select(Date, onset, offset) %>%
    distinct()

  # Store the onset and offset times in the list
  onset_offset_times[[cage]] <- onset_offset
}

# Combine the onset and offset times into a single data frame for easier viewing
onset_offset_combined <- bind_rows(onset_offset_times, .id = "Cage")

# Display the onset and offset times
print(onset_offset_combined)



# Function to get onset and offset times for a specific cage
get_times <- function(cage_number) {
  onset_offset_times[[as.character(cage_number)]]
}

# Example: Get onset and offset times for cage 20
time_cage_20 <- get_times(20)
print(time_cage_20)

print(get_times(15))


##################

#Selected an activity threshold of 30 minutes

library(zoo)
library(ggplot2)
library(dplyr)

# Function to determine activity periods with a sustained activity threshold
determine_activity_periods <- function(df_day) {
  # Calculate the 20-minute rolling mean
  df_day <- df_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 50, fill = NA, align = "center"))

  # Fill NA values with 0 for rolling_mean
  df_day$rolling_mean[is.na(df_day$rolling_mean)] <- 0

  # Calculate the daily mean activity
  daily_mean <- mean(df_day$HopsPerMinute, na.rm = TRUE)
  df_day$daily_mean <- daily_mean

  # Find the intersection points
  df_day <- df_day %>%
    mutate(intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                                (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE))

  # Ensure the 'intersect' column is logical (TRUE/FALSE)
  df_day$intersect <- as.logical(df_day$intersect)

  # Identify the periods of sustained activity
  df_day <- df_day %>%
    mutate(activity_period = cumsum(c(TRUE, diff(intersect) != 0)))

  # Filter out short activity periods
  activity_periods <- df_day %>%
    group_by(activity_period) %>%
    summarise(start = min(timestamp, na.rm = TRUE),
              end = max(timestamp, na.rm = TRUE),
              duration = as.numeric(difftime(max(timestamp, na.rm = TRUE), min(timestamp, na.rm = TRUE), units = "mins")))

  # Mark the filtered activity periods back in the main dataframe
  df_day <- df_day %>%
    left_join(activity_periods, by = "activity_period") %>%
    mutate(onset = if_else(timestamp == start, timestamp, as.POSIXct(NA)),
           offset = if_else(timestamp == end, timestamp, as.POSIXct(NA))) %>%
    filter(!is.na(start)) %>%
    select(-start, -end, -duration)

  return(df_day)
}

# Test the function on Cage 2 with a 30-minute threshold
Cage_2 <- Cage_2 %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

results <- list()

for (date in unique(Cage_2$Date)) {
  df_day <- Cage_2 %>%
    filter(Date == date)

  df_day <- determine_activity_periods(df_day)

  results[[as.character(date)]] <- df_day
}

df_combined_cage_2 <- bind_rows(results)

# Plot the results for Cage 2
ggplot(df_combined_cage_2, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_combined_cage_2 %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_point(data = df_combined_cage_2 %>% filter(!is.na(onset) | !is.na(offset)), aes(y = rolling_mean), color = "blue", size = 3) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Interpolated Crossings for Cage 2", x = "Time", y = "Rolling Average") +
  theme_classic()

###############


library(zoo)
library(ggplot2)
library(dplyr)

# Function to parse lights on and off times
parse_lights_time <- function(df, column) {
  df %>%
    mutate(time = as.POSIXct(paste0(Date, " ", !!sym(column)), format = "%Y-%m-%d %I:%M %p"))
}

# Apply the parsing function to lights on and off times
df1 <- df %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d"))) %>%
  parse_lights_time("LightsOn") %>%
  rename(LightsOnTime = time) %>%
  parse_lights_time("LightsOff") %>%
  rename(LightsOffTime = time)

# Filter data for a specific cage (e.g., Cage 2)
df_cage_2 <- df1 %>%
  filter(Cage == 2)

# Calculate the rolling mean and identify sustained activity periods
df_cage_2 <- df_cage_2 %>%
  group_by(Date) %>%
  mutate(
    rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"),
    rolling_mean = ifelse(is.na(rolling_mean), 0, rolling_mean),
    daily_mean = mean(HopsPerMinute, na.rm = TRUE),
    intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                         (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE),
    intersect = as.logical(intersect),
    activity_period = cumsum(c(TRUE, diff(intersect) != 0))
  ) %>%
  ungroup()

# Function to calculate logical onset and offset relative to lights on and off
calculate_onset_offset <- function(df) {
  df %>%
    group_by(Date) %>%
    summarise(
      onset = min(timestamp[rolling_mean > daily_mean & timestamp > LightsOnTime], na.rm = TRUE),
      offset = max(timestamp[rolling_mean > daily_mean & timestamp < LightsOffTime], na.rm = TRUE)
    ) %>%
    mutate(
      onset = ifelse(is.infinite(onset), NA, as.POSIXct(onset, origin = "1970-01-01")),
      offset = ifelse(is.infinite(offset), NA, as.POSIXct(offset, origin = "1970-01-01"))
    )
}

# Apply the function to the cage data
onset_offset_times <- calculate_onset_offset(df_cage_2)

# Convert Unix timestamps to POSIXct format
onset_offset_times <- onset_offset_times %>%
  mutate(
    onset = as.POSIXct(onset, origin = "1970-01-01", tz = "UTC"),
    offset = as.POSIXct(offset, origin = "1970-01-01", tz = "UTC")
  )


# Print out the onset and offset times in a readable format
print(onset_offset_times)

# Merge the onset and offset times back to the original dataframe for plotting
df_cage_2 <- df_cage_2 %>%
  left_join(onset_offset_times, by = "Date")

# Plot the results
ggplot(df_cage_2, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_cage_2 %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_point(data = df_cage_2 %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Logical Onset/Offset Relative to Lights On/Off for Cage 2", x = "Time", y = "Rolling Average") +
  theme_classic()


###############

library(zoo)
library(ggplot2)
library(dplyr)

# Function to parse lights on and off times
parse_lights_time <- function(df, column) {
  df %>%
    mutate(time = as.POSIXct(paste0(Date, " ", !!sym(column)), format = "%Y-%m-%d %I:%M %p"))
}

# Apply the parsing function to lights on and off times
df1 <- df %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d"))) %>%
  parse_lights_time("LightsOn") %>%
  rename(LightsOnTime = time) %>%
  parse_lights_time("LightsOff") %>%
  rename(LightsOffTime = time)

# Filter data for a specific cage (e.g., Cage 2)
df_cage_2 <- df1 %>%
  filter(Cage == 2)

# Calculate the rolling mean and identify sustained activity periods
df_cage_2 <- df_cage_2 %>%
  group_by(Date) %>%
  mutate(
    rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"),
    rolling_mean = ifelse(is.na(rolling_mean), 0, rolling_mean),
    daily_mean = mean(HopsPerMinute, na.rm = TRUE),
    intersect = ifelse((lag(rolling_mean) < daily_mean & rolling_mean >= daily_mean) |
                         (lag(rolling_mean) > daily_mean & rolling_mean <= daily_mean), TRUE, FALSE),
    intersect = as.logical(intersect),
    activity_period = cumsum(c(TRUE, diff(intersect) != 0))
  ) %>%
  ungroup()

# Function to calculate logical onset and offset relative to lights on and off
calculate_onset_offset <- function(df) {
  df %>%
    group_by(Date) %>%
    summarise(
      onset = min(timestamp[rolling_mean > daily_mean & timestamp >= LightsOnTime], na.rm = TRUE),
      offset = max(timestamp[rolling_mean > daily_mean & timestamp <= LightsOffTime], na.rm = TRUE)
    ) %>%
    mutate(
      onset = ifelse(is.infinite(onset), NA, onset),
      offset = ifelse(is.infinite(offset), NA, offset)
    )
}

# Apply the function to the cage data
onset_offset_times <- calculate_onset_offset(df_cage_2)

# Convert Unix timestamps to POSIXct format
onset_offset_times <- onset_offset_times %>%
  mutate(
    onset = as.POSIXct(onset, origin = "1970-01-01", tz = "UTC"),
    offset = as.POSIXct(offset, origin = "1970-01-01", tz = "UTC")
  )

# Print out the onset and offset times in a readable format
print(onset_offset_times)

# Merge the onset and offset times back to the original dataframe for plotting
df_cage_2 <- df_cage_2 %>%
  left_join(onset_offset_times, by = "Date")

# Plot the results
ggplot(df_cage_2, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_cage_2 %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_point(data = df_cage_2 %>% filter(timestamp == onset | timestamp == offset), aes(y = rolling_mean), color = "blue", size = 3) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Logical Onset/Offset Relative to Lights On/Off for Cage 2", x = "Time", y = "Rolling Average") +
  theme_classic()

#########################

library(zoo)
library(ggplot2)
library(dplyr)

# Filter data for Cage 2 on March 7th, 2023
df_cage_2_day <- df %>%
  filter(Cage == 2 & format(timestamp, "%Y-%m-%d") == "2023-03-11")

# Calculate the rolling mean for the day
df_cage_2_day <- df_cage_2_day %>%
  mutate(
    rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"),
    rolling_mean = ifelse(is.na(rolling_mean), 0, rolling_mean),
    daily_mean = mean(HopsPerMinute, na.rm = TRUE)
  )

# Calculate the lights on and off times
df_cage_2_day <- df_cage_2_day %>%
  mutate(
    LightsOnTime = as.POSIXct(paste0(Date, " ", LightsOn), format = "%Y-%m-%d %I:%M %p"),
    LightsOffTime = as.POSIXct(paste0(Date, " ", LightsOff), format = "%Y-%m-%d %I:%M %p")
  )

# Define buffer period (e.g., 3 hours before lights on and after lights off)
buffer_hours <- 3
buffer_seconds <- buffer_hours * 3600
buffer_period_before_on <- df_cage_2_day$LightsOnTime[1] - buffer_seconds
buffer_period_after_off <- df_cage_2_day$LightsOffTime[1] + buffer_seconds

# Determine the activity onset and offset times with buffer periods
onset_time <- df_cage_2_day %>%
  filter(timestamp >= buffer_period_before_on & rolling_mean > daily_mean) %>%
  summarise(onset = min(timestamp, na.rm = TRUE)) %>%
  pull(onset)

offset_time <- df_cage_2_day %>%
  filter(timestamp <= buffer_period_after_off & rolling_mean > daily_mean) %>%
  summarise(offset = max(timestamp, na.rm = TRUE)) %>%
  pull(offset)

# Print the onset and offset times
print(paste("Onset time:", onset_time))
print(paste("Offset time:", offset_time))

# Plot the results
ggplot(df_cage_2_day, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(yintercept = df_cage_2_day$daily_mean, linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(df_cage_2_day$LightsOnTime), linetype = "dotted", color = "orange", size = 1.8) +
  geom_vline(xintercept = as.numeric(df_cage_2_day$LightsOffTime), linetype = "dotted", color = "black", size = 1.8) +
  geom_point(aes(x = onset_time, y = df_cage_2_day$rolling_mean[which.min(abs(df_cage_2_day$timestamp - onset_time))]), color = "blue", size = 3) +
  geom_point(aes(x = offset_time, y = df_cage_2_day$rolling_mean[which.min(abs(df_cage_2_day$timestamp - offset_time))]), color = "blue", size = 3) +
  labs(title = "Rolling Average and Logical Onset/Offset Relative to Lights On/Off for Cage 2 on March 7th, 2023", x = "Time", y = "Rolling Average") +
  theme_classic()
############################

library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Filter data for Cage 20 on March 5th, 2023
df1 <- df %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

df_cage_day <- df1 %>%
  filter(Cage == 20 & Date == as.Date("2023-03-05"))

# Calculate the rolling mean
df_cage_day <- df_cage_day %>%
  mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = NA, align = "center"))

# Calculate the daily mean activity
daily_mean <- mean(df_cage_day$HopsPerMinute, na.rm = TRUE)
df_cage_day$daily_mean <- daily_mean

# Calculate Center of Gravity (CoG)
cog_time <- df_cage_day %>%
  filter(!is.na(rolling_mean)) %>%
  summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
  pull(cog)
cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

# Calculate cycle length (assuming 24-hour cycle)
cycle_length <- 24 * 3600  # in seconds
half_cycle <- cycle_length / 2

# Onset: Move back from CoG to find the first point where activity exceeds the average
onset_time <- df_cage_day %>%
  filter(timestamp <= cog_time) %>%
  arrange(desc(timestamp)) %>%
  filter(rolling_mean > daily_mean) %>%
  summarise(onset = first(timestamp)) %>%
  pull(onset)

# Offset: Move forward from CoG to find the last point where activity exceeds the average
offset_time <- df_cage_day %>%
  filter(timestamp >= cog_time) %>%
  arrange(timestamp) %>%
  filter(rolling_mean > daily_mean) %>%
  summarise(offset = last(timestamp)) %>%
  pull(offset)

# Plot the results
ggplot(df_cage_day, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(yintercept = daily_mean, linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(cog_time), linetype = "dotted", color = "blue") +
  geom_point(aes(x = onset_time, y = df_cage_day$rolling_mean[which.min(abs(df_cage_day$timestamp - onset_time))]), color = "blue", size = 3) +
  geom_point(aes(x = offset_time, y = df_cage_day$rolling_mean[which.min(abs(df_cage_day$timestamp - offset_time))]), color = "blue", size = 3) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20 on March 5th, 2023", x = "Time", y = "Rolling Average") +
  theme_classic()

################

library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Filter data for Cage 20 on March 5th, 2023
df1 <- df %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

df_cage_day <- df1 %>%
  filter(Cage == 20 & Date == as.Date("2023-03-05"))

# Calculate the rolling mean
df_cage_day <- df_cage_day %>%
  mutate(rolling_mean = rollmean(HopsPerMinute, k = 10, fill = 0, align = "center"))

# Calculate the daily mean activity
daily_mean <- mean(df_cage_day$HopsPerMinute, na.rm = TRUE)
df_cage_day$daily_mean <- daily_mean

# Calculate Center of Gravity (CoG)
cog_time <- df_cage_day %>%
  filter(!is.na(rolling_mean)) %>%
  summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
  pull(cog)
cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

# Calculate cycle length (assuming 24-hour cycle)
cycle_length <- 24 * 60 * 60  # in seconds
half_cycle <- cycle_length / 2








# Define a function to find the valid onset and offset times
find_valid_time <- function(df, cog_time, half_cycle, daily_mean, direction = "backward", duration_threshold = 30 * 60) {
  if (direction == "backward") {
    df_filtered <- df %>%
      filter(timestamp <= (cog_time - half_cycle)) %>%
      arrange(desc(timestamp))
  } else {
    df_filtered <- df %>%
      filter(timestamp >= (cog_time + half_cycle)) %>%
      arrange(timestamp)
  }

  print(paste("Filtering direction:", direction))
  print(head(df_filtered))

  consecutive_above_mean <- 0
  for (i in 1:nrow(df_filtered)) {
    if (df_filtered$rolling_mean[i] > daily_mean) {
      consecutive_above_mean <- consecutive_above_mean + 1
      print(paste("Consecutive above mean:", consecutive_above_mean))
      if (consecutive_above_mean * 60 >= duration_threshold) {
        return(df_filtered$timestamp[i])
      }
    } else {
      consecutive_above_mean <- 0
    }
  }
  return(NA)
}

# Onset: Move back from CoG to find the first point where activity exceeds the average for 30 minutes
onset_time <- find_valid_time(df_cage_day, cog_time, half_cycle, daily_mean, "backward")

# Offset: Move forward from CoG to find the last point where activity exceeds the average for 30 minutes
offset_time <- find_valid_time(df_cage_day, cog_time, half_cycle, daily_mean, "forward")












# Plot the results
ggplot(df_cage_day, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(yintercept = daily_mean, linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(cog_time), linetype = "dotted", color = "blue") +
  geom_point(aes(x = onset_time, y = df_cage_day$rolling_mean[which.min(abs(df_cage_day$timestamp - onset_time))]), color = "blue", size = 3) +
  geom_point(aes(x = offset_time, y = df_cage_day$rolling_mean[which.min(abs(df_cage_day$timestamp - offset_time))]), color = "blue", size = 3) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20 on March 5th, 2023", x = "Time", y = "Rolling Average") +
  theme_classic()

# Print the calculated times
print(paste("CoG time:", cog_time))
print(paste("Onset time:", onset_time))
print(paste("Offset time:", offset_time))






# Function to find onset and offset times based on sustained activity above the mean for a given duration
find_onset_offset <- function(df, cog_time, half_cycle, daily_mean, duration_threshold = 30 * 60) {
  df_before_cog <- df %>%
    filter(timestamp <= (cog_time - half_cycle)) %>%
    arrange(timestamp)

  df_after_cog <- df %>%
    filter(timestamp >= (cog_time + half_cycle)) %>%
    arrange(timestamp)

  onset_time <- df_before_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(onset = min(timestamp, na.rm = TRUE)) %>%
    pull(onset)

  offset_time <- df_after_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(offset = max(timestamp, na.rm = TRUE)) %>%
    pull(offset)

  return(list(onset = onset_time, offset = offset_time))
}

# Find the onset and offset times
times <- find_onset_offset(df_cage_day, cog_time, half_cycle, daily_mean)
onset_time <- times$onset
offset_time <- times$offset














library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Filter data for Cage 20 on March 5th, 2023
df1 <- df %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

df_cage_day <- df1 %>%
  filter(Cage == 20 & Date == as.Date("2023-03-09"))

# Calculate the rolling mean with a window size of 20
df_cage_day <- df_cage_day %>%
  mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = 0, align = "center"))

# Calculate the daily mean activity
daily_mean <- mean(df_cage_day$HopsPerMinute, na.rm = TRUE)
df_cage_day$daily_mean <- daily_mean

# Calculate Center of Gravity (CoG)
cog_time <- df_cage_day %>%
  filter(!is.na(rolling_mean)) %>%
  summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
  pull(cog)
cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

# Calculate cycle length (adjusted for the rolling mean window)
cycle_length <- 24 * 20 * 60  # 24-hour cycle in seconds
half_cycle <- cycle_length / 2

# Function to find onset and offset times based on sustained activity above the mean for a given duration
find_onset_offset <- function(df, cog_time, half_cycle, daily_mean, duration_threshold = 20 * 60) {
  df_before_cog <- df %>%
    filter(timestamp <= (cog_time - half_cycle)) %>%
    arrange(timestamp)

  df_after_cog <- df %>%
    filter(timestamp >= (cog_time + half_cycle)) %>%
    arrange(timestamp)

  onset_time <- df_before_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(onset = max(timestamp, na.rm = TRUE)) %>%
    pull(onset)

  offset_time <- df_after_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(offset = min(timestamp, na.rm = TRUE)) %>%
    pull(offset)

  return(list(onset = onset_time, offset = offset_time))
}

# Find the onset and offset times
times <- find_onset_offset(df_cage_day, cog_time, half_cycle, daily_mean)
onset_time <- times$onset
offset_time <- times$offset

# Plot the results
ggplot(df_cage_day, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(yintercept = daily_mean, linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(cog_time), linetype = "dotted", color = "blue") +
  geom_point(aes(x = onset_time, y = df_cage_day$rolling_mean[which.min(abs(df_cage_day$timestamp - onset_time))]), color = "blue", size = 3) +
  geom_point(aes(x = offset_time, y = df_cage_day$rolling_mean[which.min(abs(df_cage_day$timestamp - offset_time))]), color = "blue", size = 3) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20 on March 5th, 2023", x = "Time", y = "Rolling Average") +
  theme_classic()

# Print the calculated times
print(paste("CoG time:", cog_time))
print(paste("Onset time:", onset_time))
print(paste("Offset time:", offset_time))


#################


library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset and offset times based on sustained activity above the mean for a given duration
find_onset_offset <- function(df, cog_time, half_cycle, daily_mean, duration_threshold = 20 * 60) {
  df_before_cog <- df %>%
    filter(timestamp <= (cog_time - half_cycle)) %>%
    arrange(desc(timestamp))

  df_after_cog <- df %>%
    filter(timestamp >= (cog_time + half_cycle)) %>%
    arrange(timestamp)

  onset_time <- df_before_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(onset = max(timestamp, na.rm = TRUE)) %>%
    pull(onset)

  offset_time <- df_after_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(offset = min(timestamp, na.rm = TRUE)) %>%
    pull(offset)

  return(list(onset = onset_time, offset = offset_time))
}

# Initialize an empty list to store results for each Cage
results_all_cages <- list()

# Loop through each unique Cage
for (cage in unique(df$Cage)) {
  # Filter data for the current Cage
  df_cage <- df %>%
    filter(Cage == cage) %>%
    mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

  # Initialize an empty list to store results for each day within the current Cage
  results <- list()

  # Loop through each unique date within the current Cage
  for (date in unique(df_cage$Date)) {
    # Filter data for the current date
    df_cage_day <- df_cage %>%
      filter(Date == date)

    # Calculate the rolling mean with a window size of 20
    df_cage_day <- df_cage_day %>%
      mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = 0, align = "center"))

    # Calculate the daily mean activity
    daily_mean <- mean(df_cage_day$HopsPerMinute, na.rm = TRUE)
    df_cage_day$daily_mean <- daily_mean

    # Calculate Center of Gravity (CoG)
    cog_time <- df_cage_day %>%
      filter(!is.na(rolling_mean)) %>%
      summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
      pull(cog)
    cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

    # Calculate cycle length (adjusted for the rolling mean window)
    cycle_length <- 24 * 20 * 60  # in seconds
    half_cycle <- cycle_length / 2

    # Find the onset and offset times
    times <- find_onset_offset(df_cage_day, cog_time, half_cycle, daily_mean)
    onset_time <- times$onset
    offset_time <- times$offset

    # Store the results for the current day
    results[[as.character(date)]] <- df_cage_day %>%
      mutate(onset_time = onset_time, offset_time = offset_time)
  }

  # Combine results into a single data frame for the current Cage
  df_combined_cage <- bind_rows(results)

  # Store the combined results for the current Cage in the main list
  results_all_cages[[as.character(cage)]] <- df_combined_cage
}

# Combine results from all Cages into a single data frame
df_combined_all <- bind_rows(results_all_cages)

# Save the results to a file or variable for later use
saveRDS(results_all_cages, file = "results_all_cages.rds")

# Function to plot the results for a specific Cage
plot_cage <- function(cage_number) {
  df_cage_to_plot <- results_all_cages[[as.character(cage_number)]]

  ggplot(df_cage_to_plot, aes(x = timestamp, y = rolling_mean)) +
    geom_line() +
    geom_hline(data = df_cage_to_plot %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
    geom_vline(data = df_cage_to_plot %>% distinct(Date, onset_time), aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
    geom_vline(data = df_cage_to_plot %>% distinct(Date, offset_time), aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
    facet_wrap(~ Date, scales = "free_x", ncol = 1) +
    labs(title = paste("Rolling Average and Onset/Offset Relative to CoG for Cage", cage_number), x = "Time", y = "Rolling Average") +
    theme_classic()
}

# Example: Plot for Cage 20
plot_cage(20)

# Initialize an empty list to store onset and offset times for each cage
onset_offset_times <- list()

# Loop through each cage in the results_all_cages list
for (cage in names(results_all_cages)) {
  df_cage <- results_all_cages[[cage]]

  # Extract the unique onset and offset times
  onset_offset <- df_cage %>%
    select(Date, onset_time, offset_time) %>%
    distinct()

  # Store the onset and offset times in the list
  onset_offset_times[[cage]] <- onset_offset
}

# Combine the onset and offset times into a single data frame for easier viewing
onset_offset_combined <- bind_rows(onset_offset_times, .id = "Cage")

# Display the onset and offset times
print(onset_offset_combined)

# Function to get onset and offset times for a specific cage
get_times <- function(cage_number) {
  onset_offset_times[[as.character(cage_number)]]
}

# Example: Get onset and offset times for cage 20
time_cage_20 <- get_times(20)
print(time_cage_20)

print(get_times(15))


##########

library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset and offset times based on sustained activity above the mean for a given duration
find_onset_offset <- function(df, cog_time, half_cycle, daily_mean, interval_minutes = 20, sustained_minutes = 30) {
  duration_threshold <- (sustained_minutes / interval_minutes) * interval_minutes * 60

  df_before_cog <- df %>%
    filter(timestamp <= (cog_time - half_cycle)) %>%
    arrange(desc(timestamp))

  df_after_cog <- df %>%
    filter(timestamp >= (cog_time + half_cycle)) %>%
    arrange(timestamp)

  onset_time <- df_before_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, interval_minutes * 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(onset = max(timestamp, na.rm = TRUE)) %>%
    pull(onset)

  offset_time <- df_after_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, interval_minutes * 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(offset = min(timestamp, na.rm = TRUE)) %>%
    pull(offset)

  return(list(onset = onset_time, offset = offset_time))
}

# Initialize an empty list to store results for each Cage
results_all_cages <- list()

# Loop through each unique Cage
for (cage in unique(df$Cage)) {
  # Filter data for the current Cage
  df_cage <- df %>%
    filter(Cage == cage) %>%
    mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

  # Initialize an empty list to store results for each day within the current Cage
  results <- list()

  # Loop through each unique date within the current Cage
  for (date in unique(df_cage$Date)) {
    # Filter data for the current date
    df_cage_day <- df_cage %>%
      filter(Date == date)

    # Calculate the rolling mean with a window size of 20
    df_cage_day <- df_cage_day %>%
      mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = 0, align = "center"))

    # Calculate the daily mean activity
    daily_mean <- mean(df_cage_day$HopsPerMinute, na.rm = TRUE)
    df_cage_day$daily_mean <- daily_mean

    # Calculate Center of Gravity (CoG)
    cog_time <- df_cage_day %>%
      filter(!is.na(rolling_mean)) %>%
      summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
      pull(cog)
    cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

    # Calculate cycle length (adjusted for the rolling mean window)
    cycle_length <- 24 * 20 * 60  # in seconds
    half_cycle <- cycle_length / 2

    # Find the onset and offset times
    times <- find_onset_offset(df_cage_day, cog_time, half_cycle, daily_mean, interval_minutes = 20, sustained_minutes = 30)
    onset_time <- times$onset
    offset_time <- times$offset

    # Store the results for the current day
    results[[as.character(date)]] <- df_cage_day %>%
      mutate(onset_time = onset_time, offset_time = offset_time)
  }

  # Combine results into a single data frame for the current Cage
  df_combined_cage <- bind_rows(results)

  # Store the combined results for the current Cage in the main list
  results_all_cages[[as.character(cage)]] <- df_combined_cage
}

# Combine results from all Cages into a single data frame
df_combined_all <- bind_rows(results_all_cages)

# Save the results to a file or variable for later use
saveRDS(results_all_cages, file = "results_all_cages.rds")

# Function to plot the results for a specific Cage
plot_cage <- function(cage_number) {
  df_cage_to_plot <- results_all_cages[[as.character(cage_number)]]

  ggplot(df_cage_to_plot, aes(x = timestamp, y = rolling_mean)) +
    geom_line() +
    geom_hline(data = df_cage_to_plot %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
    geom_vline(data = df_cage_to_plot %>% distinct(Date, onset_time), aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
    geom_vline(data = df_cage_to_plot %>% distinct(Date, offset_time), aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
    facet_wrap(~ Date, scales = "free_x", ncol = 1) +
    labs(title = paste("Rolling Average and Onset/Offset Relative to CoG for Cage", cage_number), x = "Time", y = "Rolling Average") +
    theme_classic()
}

# Example: Plot for Cage 20
plot_cage(7)

# Initialize an empty list to store onset and offset times for each cage
onset_offset_times <- list()

# Loop through each cage in the results_all_cages list
for (cage in names(results_all_cages)) {
  df_cage <- results_all_cages[[cage]]

  # Extract the unique onset and offset times
  onset_offset <- df_cage %>%
    select(Date, onset_time, offset_time) %>%
    distinct()

  # Store the onset and offset times in the list
  onset_offset_times[[cage]] <- onset_offset
}

# Combine the onset and offset times into a single data frame for easier viewing
onset_offset_combined <- bind_rows(onset_offset_times, .id = "Cage")

# Display the onset and offset times
print(onset_offset_combined)

# Function to get onset and offset times for a specific cage
get_times <- function(cage_number) {
  onset_offset_times[[as.character(cage_number)]]
}

# Example: Get onset and offset times for cage 20
time_cage_20 <- get_times(20)
print(time_cage_20)

print(get_times(7))



########

# 10 minute rolling mean #WORSE
# trying 30 miuntes

#######

library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset and offset times based on sustained activity above the mean for a given duration
find_onset_offset <- function(df, cog_time, half_cycle, daily_mean, interval_minutes = 30, sustained_minutes = 45) {
  duration_threshold <- (sustained_minutes / interval_minutes) * interval_minutes * 60

  df_before_cog <- df %>%
    filter(timestamp <= (cog_time - half_cycle)) %>%
    arrange(desc(timestamp))

  df_after_cog <- df %>%
    filter(timestamp >= (cog_time + half_cycle)) %>%
    arrange(timestamp)

  onset_time <- df_before_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, interval_minutes * 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(onset = max(timestamp, na.rm = TRUE)) %>%
    pull(onset)

  offset_time <- df_after_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, interval_minutes * 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(offset = min(timestamp, na.rm = TRUE)) %>%
    pull(offset)

  return(list(onset = onset_time, offset = offset_time))
}

# Initialize an empty list to store results for each Cage
results_all_cages <- list()

# Loop through each unique Cage
for (cage in unique(df$Cage)) {
  # Filter data for the current Cage
  df_cage <- df %>%
    filter(Cage == cage) %>%
    mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

  # Initialize an empty list to store results for each day within the current Cage
  results <- list()

  #Show progress
  print(cage)

  # Loop through each unique date within the current Cage
  for (date in unique(df_cage$Date)) {
    # Filter data for the current date
    df_cage_day <- df_cage %>%
      filter(Date == date)

    # Calculate the rolling mean with a window size of 30
    df_cage_day <- df_cage_day %>%
      mutate(rolling_mean = rollmean(HopsPerMinute, k = 30, fill = 0, align = "center"))

    # Calculate the daily mean activity
    daily_mean <- mean(df_cage_day$HopsPerMinute, na.rm = TRUE)
    df_cage_day$daily_mean <- daily_mean

    # Calculate Center of Gravity (CoG)
    cog_time <- df_cage_day %>%
      filter(!is.na(rolling_mean)) %>%
      summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
      pull(cog)
    cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

    # Calculate cycle length (adjusted for the rolling mean window)
    cycle_length <- 24 * 30 * 60  # in seconds
    half_cycle <- cycle_length / 2

    # Find the onset and offset times
    times <- find_onset_offset(df_cage_day, cog_time, half_cycle, daily_mean, interval_minutes = 30, sustained_minutes = 30)
    onset_time <- times$onset
    offset_time <- times$offset

    # Store the results for the current day
    results[[as.character(date)]] <- df_cage_day %>%
      mutate(onset_time = onset_time, offset_time = offset_time)
  }

  # Combine results into a single data frame for the current Cage
  df_combined_cage <- bind_rows(results)

  # Store the combined results for the current Cage in the main list
  results_all_cages[[as.character(cage)]] <- df_combined_cage
}

# Combine results from all Cages into a single data frame
df_combined_all <- bind_rows(results_all_cages)

# Example: Plot for Cage 20
plot_cage(20)


# Save the results to a file or variable for later use
#saveRDS(results_all_cages, file = "results_all_cages.rds")

# Function to plot the results for a specific Cage
plot_cage <- function(cage_number) {
  df_cage_to_plot <- results_all_cages[[as.character(cage_number)]]

  ggplot(df_cage_to_plot, aes(x = timestamp, y = rolling_mean)) +
    geom_line() +
    geom_hline(data = df_cage_to_plot %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
    geom_vline(data = df_cage_to_plot %>% distinct(Date, onset_time), aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
    geom_vline(data = df_cage_to_plot %>% distinct(Date, offset_time), aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
    facet_wrap(~ Date, scales = "free_x", ncol = 1) +
    labs(title = paste("Rolling Average and Onset/Offset Relative to CoG for Cage", cage_number), x = "Time", y = "Rolling Average") +
    theme_classic()
}

# Example: Plot for Cage 20
plot_cage(20)

# Initialize an empty list to store onset and offset times for each cage
onset_offset_times <- list()

# Loop through each cage in the results_all_cages list
for (cage in names(results_all_cages)) {
  df_cage <- results_all_cages[[cage]]

  # Extract the unique onset and offset times
  onset_offset <- df_cage %>%
    select(Date, onset_time, offset_time) %>%
    distinct()

  # Store the onset and offset times in the list
  onset_offset_times[[cage]] <- onset_offset
}

# Combine the onset and offset times into a single data frame for easier viewing
onset_offset_combined <- bind_rows(onset_offset_times, .id = "Cage")

# Display the onset and offset times
print(onset_offset_combined)

# Function to get onset and offset times for a specific cage
get_times <- function(cage_number) {
  onset_offset_times[[as.character(cage_number)]]
}

# Example: Get onset and offset times for cage 20
time_cage_20 <- get_times(20)
print(time_cage_20)

print(get_times(15))


## Also doesn't work. I think the math is wrong

########################

library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset and offset times based on sustained activity above the mean for a given duration
find_onset_offset <- function(df, cog_time, half_cycle, daily_mean, interval_minutes = 30, sustained_minutes = 45) {
  duration_threshold <- (sustained_minutes / interval_minutes) * interval_minutes * 60

  df_before_cog <- df %>%
    filter(timestamp <= (cog_time - half_cycle)) %>%
    arrange(desc(timestamp))

  df_after_cog <- df %>%
    filter(timestamp >= (cog_time + half_cycle)) %>%
    arrange(timestamp)

  onset_time <- df_before_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, interval_minutes * 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(onset = max(timestamp, na.rm = TRUE)) %>%
    pull(onset)

  offset_time <- df_after_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, interval_minutes * 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(offset = min(timestamp, na.rm = TRUE)) %>%
    pull(offset)

  return(list(onset = onset_time, offset = offset_time))
}

# Initialize an empty list to store results for Cage 20
results <- list()

# Filter data for Cage 20
df_cage <- df %>%
  filter(Cage == 20) %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Checkpoint 1: Ensure df_cage contains data for Cage 20
print(head(df_cage))

# Loop through each unique date within Cage 20
for (date in unique(df_cage$Date)) {
  # Filter data for the current date
  df_cage_day <- df_cage %>%
    filter(Date == date)

  # Calculate the rolling mean with a window size of 30
  df_cage_day <- df_cage_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = 0, align = "center"))

  # Calculate the daily mean activity
  daily_mean <- df_cage_day %>%
    filter(timestamp >= as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p") &
             timestamp <= as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p")) %>%
    summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE)) %>%
    pull(mean_activity)
  df_cage_day$daily_mean <- daily_mean

  # Checkpoint 2: Ensure daily_mean is calculated correctly
  print(paste("Date:", date, "Daily Mean:", daily_mean))

  # Calculate Center of Gravity (CoG)
  cog_time <- df_cage_day %>%
    filter(!is.na(rolling_mean)) %>%
    summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
    pull(cog)
  cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

  # Checkpoint 3: Ensure CoG time is calculated correctly
  print(paste("Date:", date, "CoG time:", cog_time))

  # Calculate cycle length (adjusted for the rolling mean window)
  cycle_length <- 24 * 20 * 60  # in seconds
  half_cycle <- cycle_length / 2

  # Find the onset and offset times
  times <- find_onset_offset(df_cage_day, cog_time, half_cycle, daily_mean, interval_minutes = 30, sustained_minutes = 30)
  onset_time <- times$onset
  offset_time <- times$offset

  # Checkpoint 4: Ensure onset and offset times are calculated correctly
  print(paste("Date:", date, "Onset time:", onset_time, "Offset time:", offset_time))

  # Store the results for the current day
  results[[as.character(date)]] <- df_cage_day %>%
    mutate(onset_time = onset_time, offset_time = offset_time)
}

# Combine results into a single data frame for Cage 20
df_combined_cage <- bind_rows(results)

# Example: Plot for Cage 20
ggplot(df_combined_cage, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_combined_cage %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size =2) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size=2) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()

################

library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset and offset times based on sustained activity above the mean for a given duration
find_onset_offset <- function(df, cog_time, half_cycle, daily_mean, interval_minutes = 30, sustained_minutes = 45) {
  duration_threshold <- sustained_minutes * 60

  df_before_cog <- df %>%
    filter(timestamp <= (cog_time - half_cycle)) %>%
    arrange(desc(timestamp))

  df_after_cog <- df %>%
    filter(timestamp >= (cog_time + half_cycle)) %>%
    arrange(timestamp)

  onset_time <- df_before_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, interval_minutes * 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(onset = max(timestamp, na.rm = TRUE)) %>%
    pull(onset)

  offset_time <- df_after_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, interval_minutes * 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(offset = min(timestamp, na.rm = TRUE)) %>%
    pull(offset)

  return(list(onset = onset_time, offset = offset_time))
}

# Initialize an empty list to store results for Cage 20
results <- list()

# Filter data for Cage 20
df_cage <- df %>%
  filter(Cage == 20) %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Checkpoint 1: Ensure df_cage contains data for Cage 20
print(head(df_cage))

# Loop through each unique date within Cage 20
for (date in unique(df_cage$Date)) {
  # Filter data for the current date
  df_cage_day <- df_cage %>%
    filter(Date == date)

  # Calculate the rolling mean with a window size of 30
  df_cage_day <- df_cage_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = 0, align = "center"))

  # Calculate the daily mean activity
  daily_mean <- df_cage_day %>%
    filter(timestamp >= as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p") &
             timestamp <= as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p")) %>%
    summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE)) %>%
    pull(mean_activity)
  df_cage_day$daily_mean <- daily_mean

  # Checkpoint 2: Ensure daily_mean is calculated correctly
  print(paste("Date:", date, "Daily Mean:", daily_mean))

  # Calculate Center of Gravity (CoG)
  cog_time <- df_cage_day %>%
    filter(!is.na(rolling_mean)) %>%
    summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
    pull(cog)
  cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

  # Checkpoint 3: Ensure CoG time is calculated correctly
  print(paste("Date:", date, "CoG time:", cog_time))

  # Calculate cycle length (adjusted for the rolling mean window)
  cycle_length <- 24 * 20 * 60  # in seconds
  half_cycle <- cycle_length / 2

  # Find the onset and offset times
  times <- find_onset_offset(df_cage_day, cog_time, half_cycle, daily_mean, interval_minutes = 30, sustained_minutes = 45)
  onset_time <- times$onset
  offset_time <- times$offset

  # Checkpoint 4: Ensure onset and offset times are calculated correctly
  print(paste("Date:", date, "Onset time:", onset_time, "Offset time:", offset_time))

  # Store the results for the current day
  results[[as.character(date)]] <- df_cage_day %>%
    mutate(onset_time = onset_time, offset_time = offset_time)
}

# Combine results into a single data frame for Cage 20
df_combined_cage <- bind_rows(results)

# Example: Plot for Cage 20
ggplot(df_combined_cage, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_combined_cage %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size=2) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size=2) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()

#################

library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset and offset times based on sustained activity above the mean for a given duration
find_onset_offset <- function(df, cog_time, half_cycle, daily_mean, interval_minutes = 10, sustained_minutes = 30) {
  duration_threshold <- sustained_minutes * 60

  df_before_cog <- df %>%
    filter(timestamp <= (cog_time - half_cycle)) %>%
    arrange(desc(timestamp))

  df_after_cog <- df %>%
    filter(timestamp >= (cog_time + half_cycle)) %>%
    arrange(timestamp)

  onset_time <- df_before_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, interval_minutes * 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(onset = max(timestamp, na.rm = TRUE)) %>%
    pull(onset)

  offset_time <- df_after_cog %>%
    mutate(cumulative_time_above_mean = cumsum(ifelse(rolling_mean > daily_mean, interval_minutes * 60, 0))) %>%
    filter(cumulative_time_above_mean >= duration_threshold) %>%
    summarise(offset = min(timestamp, na.rm = TRUE)) %>%
    pull(offset)

  return(list(onset = onset_time, offset = offset_time))
}

# Initialize an empty list to store results for Cage 20
results <- list()

# Filter data for Cage 20
df_cage <- df %>%
  filter(Cage == 20) %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Checkpoint 1: Ensure df_cage contains data for Cage 20
print(head(df_cage))

# Loop through each unique date within Cage 20
for (date in unique(df_cage$Date)) {
  # Filter data for the current date
  df_cage_day <- df_cage %>%
    filter(Date == date)

  # Calculate the rolling mean with a window size of 20
  df_cage_day <- df_cage_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = 0, align = "center"))

  # Calculate the daily mean activity
  daily_mean <- df_cage_day %>%
    filter(timestamp >= as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p") &
             timestamp <= as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p")) %>%
    summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE)) %>%
    pull(mean_activity)
  df_cage_day$daily_mean <- daily_mean

  # Checkpoint 2: Ensure daily_mean is calculated correctly
  print(paste("Date:", date, "Daily Mean:", daily_mean))

  # Calculate Center of Gravity (CoG)
  cog_time <- df_cage_day %>%
    filter(!is.na(rolling_mean)) %>%
    summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
    pull(cog)
  cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

  # Checkpoint 3: Ensure CoG time is calculated correctly
  print(paste("Date:", date, "CoG time:", cog_time))

  # Calculate cycle length (adjusted for the rolling mean window)
  cycle_length <- 24 * 20 * 60  # in seconds
  half_cycle <- cycle_length / 2

  # Find the onset and offset times
  times <- find_onset_offset(df_cage_day, cog_time, half_cycle, daily_mean, interval_minutes = 20, sustained_minutes = 30)
  onset_time <- times$onset
  offset_time <- times$offset

  # Checkpoint 4: Ensure onset and offset times are calculated correctly
  print(paste("Date:", date, "Onset time:", onset_time, "Offset time:", offset_time))

  # Store the results for the current day
  results[[as.character(date)]] <- df_cage_day %>%
    mutate(onset_time = onset_time, offset_time = offset_time)
}

# Combine results into a single data frame for Cage 20
df_combined_cage <- bind_rows(results)

# Example: Plot for Cage 20
ggplot(df_combined_cage, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_combined_cage %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size=2) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size=2) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()


###############


library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset and offset times based on sustained activity above/below the mean for a given duration
find_onset_offset <- function(df, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30) {
  duration_threshold <- sustained_minutes * 60

  # Onset: first time activity reaches above the mean and stays above for sustained duration before CoG
  df_before_cog <- df %>%
    filter(timestamp <= cog_time) %>%
    arrange(timestamp)

  cumulative_time_above_mean <- 0
  onset_time <- NA
  for (i in seq_len(nrow(df_before_cog))) {
    if (df_before_cog$rolling_mean[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + interval_minutes * 60
      if (cumulative_time_above_mean >= duration_threshold) {
        onset_time <- df_before_cog$timestamp[i]
        break
      }
    } else {
      cumulative_time_above_mean <- 0
    }
  }

  # Offset: first time activity goes below the mean and stays below for sustained duration after CoG
  df_after_cog <- df %>%
    filter(timestamp >= cog_time) %>%
    arrange(timestamp)

  cumulative_time_below_mean <- 0
  offset_time <- NA
  for (i in seq_len(nrow(df_after_cog))) {
    if (df_after_cog$rolling_mean[i] < daily_mean) {
      cumulative_time_below_mean <- cumulative_time_below_mean + interval_minutes * 60
      if (cumulative_time_below_mean >= duration_threshold) {
        offset_time <- df_after_cog$timestamp[i]
        break
      }
    } else {
      cumulative_time_below_mean <- 0
    }
  }

  return(list(onset = onset_time, offset = offset_time))
}

# Initialize an empty list to store results for Cage 20
results <- list()

# Filter data for Cage 20
df_cage <- df %>%
  filter(Cage == 20) %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Checkpoint 1: Ensure df_cage contains data for Cage 20
print(head(df_cage))

# Loop through each unique date within Cage 20
for (date in unique(df_cage$Date)) {
  # Filter data for the current date
  df_cage_day <- df_cage %>%
    filter(Date == date)

  # Calculate the rolling mean with a window size of 10
  df_cage_day <- df_cage_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 10, fill = 0, align = "center"))

  # Calculate the daily mean activity
  daily_mean <- df_cage_day %>%
    filter(timestamp >= as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p") &
             timestamp <= as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p")) %>%
    summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE)) %>%
    pull(mean_activity)
  df_cage_day$daily_mean <- daily_mean

  # Checkpoint 2: Ensure daily_mean is calculated correctly
  print(paste("Date:", date, "Daily Mean:", daily_mean))

  # Calculate Center of Gravity (CoG)
  cog_time <- df_cage_day %>%
    filter(!is.na(rolling_mean)) %>%
    summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
    pull(cog)
  cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

  # Checkpoint 3: Ensure CoG time is calculated correctly
  print(paste("Date:", date, "CoG time:", cog_time))

  # Find the onset and offset times
  times <- find_onset_offset(df_cage_day, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30)
  onset_time <- times$onset
  offset_time <- times$offset

  # Checkpoint 4: Ensure onset and offset times are calculated correctly
  print(paste("Date:", date, "Onset time:", onset_time, "Offset time:", offset_time))

  # Store the results for the current day
  results[[as.character(date)]] <- df_cage_day %>%
    mutate(onset_time = onset_time, offset_time = offset_time)
}

# Combine results into a single data frame for Cage 20
df_combined_cage <- bind_rows(results)

# Example: Plot for Cage 20
ggplot(df_combined_cage, aes(x = timestamp, y = rolling_mean)) +
  geom_line() +
  geom_hline(data = df_combined_cage %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()


#######################


library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset and offset times based on sustained activity above/below the mean for a given duration
find_onset_offset <- function(df, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30) {
  duration_threshold <- sustained_minutes * 60

  # Onset: first time activity reaches above the mean and stays above for sustained duration before CoG
  df_before_cog <- df %>%
    filter(timestamp <= cog_time) %>%
    arrange(timestamp)

  cumulative_time_above_mean <- 0
  onset_time <- NA
  for (i in seq_len(nrow(df_before_cog))) {
    if (df_before_cog$rolling_mean[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + interval_minutes * 60
      if (cumulative_time_above_mean >= duration_threshold) {
        # Check if the activity falls back below the mean
        if (all(df_before_cog$rolling_mean[i:(i + duration_threshold / (interval_minutes * 60))] > daily_mean, na.rm = TRUE)) {
          onset_time <- df_before_cog$timestamp[i]
          break
        }
      }
    } else {
      cumulative_time_above_mean <- 0
    }
  }

  # Offset: first time activity goes below the mean and stays below for sustained duration after CoG
  df_after_cog <- df %>%
    filter(timestamp >= cog_time) %>%
    arrange(timestamp)

  cumulative_time_below_mean <- 0
  offset_time <- NA
  for (i in seq_len(nrow(df_after_cog))) {
    if (df_after_cog$rolling_mean[i] < daily_mean) {
      cumulative_time_below_mean <- cumulative_time_below_mean + interval_minutes * 60
      if (cumulative_time_below_mean >= duration_threshold) {
        # Check if the activity remains below the mean
        if (all(df_after_cog$rolling_mean[i:(i + duration_threshold / (interval_minutes * 60))] < daily_mean, na.rm = TRUE)) {
          offset_time <- df_after_cog$timestamp[i]
          break
        }
      }
    } else {
      cumulative_time_below_mean <- 0
    }
  }

  return(list(onset = onset_time, offset = offset_time))
}

# Initialize an empty list to store results for Cage 20
results <- list()

# Filter data for Cage 20
df_cage <- df %>%
  filter(Cage == 2) %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Checkpoint 1: Ensure df_cage contains data for Cage 20
print(head(df_cage))

# Loop through each unique date within Cage 20
for (date in unique(df_cage$Date)) {
  # Filter data for the current date
  df_cage_day <- df_cage %>%
    filter(Date == date)

  # Calculate the rolling mean with a window size of 10
  df_cage_day <- df_cage_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 10, fill = 0, align = "center"))

  # Calculate the daily mean activity
  daily_mean <- df_cage_day %>%
    filter(timestamp >= as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p") &
             timestamp <= as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p")) %>%
    summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE)) %>%
    pull(mean_activity)
  df_cage_day$daily_mean <- daily_mean

  # Checkpoint 2: Ensure daily_mean is calculated correctly
  print(paste("Date:", date, "Daily Mean:", daily_mean))

  # Calculate Center of Gravity (CoG)
  cog_time <- df_cage_day %>%
    filter(!is.na(rolling_mean)) %>%
    summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
    pull(cog)
  cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

  # Checkpoint 3: Ensure CoG time is calculated correctly
  print(paste("Date:", date, "CoG time:", cog_time))

  # Find the onset and offset times
  times <- find_onset_offset(df_cage_day, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30)
  onset_time <- times$onset
  offset_time <- times$offset

  # Checkpoint 4: Ensure onset and offset times are calculated correctly
  print(paste("Date:", date, "Onset time:", onset_time, "Offset time:", offset_time))

  # Store the results for the current day
  results[[as.character(date)]] <- df_cage_day %>%
    mutate(onset_time = onset_time, offset_time = offset_time)
}

# Combine results into a single data frame for Cage 20
df_combined_cage <- bind_rows(results)

# Example: Plot for Cage 20 with grey background for lights off periods
ggplot(df_combined_cage, aes(x = timestamp, y = rolling_mean)) +
  geom_rect(data = df_combined_cage %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = start_time, xmax = as.POSIXct(paste(Date, "09:00"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = df_combined_cage %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = as.POSIXct(paste(Date, "21:00"), format="%Y-%m-%d %H:%M"), xmax = start_time + days(1), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_line() +
  geom_hline(data = df_combined_cage %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()



###########################
library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset and offset times based on sustained activity above/below the mean for a given duration
find_onset_offset <- function(df, cog_time, daily_mean, interval_minutes = 10, sustained_minutes_onset = 30, sustained_minutes_offset = 60) {
  duration_threshold_onset <- sustained_minutes_onset * 60
  duration_threshold_offset <- sustained_minutes_offset * 60

  # Onset: first time activity reaches above the mean and stays above for sustained duration before CoG
  df_before_cog <- df %>%
    filter(timestamp <= cog_time) %>%
    arrange(timestamp)

  cumulative_time_above_mean <- 0
  onset_time <- NA
  for (i in seq_len(nrow(df_before_cog))) {
    if (df_before_cog$rolling_mean[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + interval_minutes * 60
      if (cumulative_time_above_mean >= duration_threshold_onset) {
        # Check if the activity falls back below the mean
        if (all(df_before_cog$rolling_mean[i:(i + duration_threshold_onset / (interval_minutes * 60))] > daily_mean, na.rm = TRUE)) {
          onset_time <- df_before_cog$timestamp[i]
          break
        }
      }
    } else {
      cumulative_time_above_mean <- 0
    }
  }

  # Offset: first time activity goes below the mean and stays below for sustained duration after CoG
  df_after_cog <- df %>%
    filter(timestamp >= cog_time) %>%
    arrange(timestamp)

  cumulative_time_below_mean <- 0
  offset_time <- NA
  for (i in seq_len(nrow(df_after_cog))) {
    if (df_after_cog$rolling_mean[i] < daily_mean) {
      cumulative_time_below_mean <- cumulative_time_below_mean + interval_minutes * 60
      if (cumulative_time_below_mean >= duration_threshold_offset) {
        # Check if the activity remains below the mean
        if (all(df_after_cog$rolling_mean[i:(i + duration_threshold_offset / (interval_minutes * 60))] < daily_mean, na.rm = TRUE)) {
          offset_time <- df_after_cog$timestamp[i]
          break
        }
      }
    } else {
      cumulative_time_below_mean <- 0
    }
  }

  return(list(onset = onset_time, offset = offset_time))
}

# Initialize an empty list to store results for Cage 20
results <- list()

# Filter data for Cage 20
df_cage <- df %>%
  filter(Cage == 20) %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Checkpoint 1: Ensure df_cage contains data for Cage 20
print(head(df_cage))

# Loop through each unique date within Cage 20
for (date in unique(df_cage$Date)) {
  # Filter data for the current date
  df_cage_day <- df_cage %>%
    filter(Date == date)

  # Calculate the rolling mean with a window size of 20
  df_cage_day <- df_cage_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 20, fill = 0, align = "center"))

  # Calculate the daily mean activity
  daily_mean <- df_cage_day %>%
    filter(timestamp >= as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p") &
             timestamp <= as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p")) %>%
    summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE)) %>%
    pull(mean_activity)
  df_cage_day$daily_mean <- daily_mean

  # Checkpoint 2: Ensure daily_mean is calculated correctly
  print(paste("Date:", date, "Daily Mean:", daily_mean))

  # Calculate Center of Gravity (CoG)
  cog_time <- df_cage_day %>%
    filter(!is.na(rolling_mean)) %>%
    summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
    pull(cog)
  cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

  # Checkpoint 3: Ensure CoG time is calculated correctly
  print(paste("Date:", date, "CoG time:", cog_time))

  # Find the onset and offset times
  times <- find_onset_offset(df_cage_day, cog_time, daily_mean, interval_minutes = 20, sustained_minutes_onset = 30, sustained_minutes_offset = 90)
  onset_time <- times$onset
  offset_time <- times$offset

  # Checkpoint 4: Ensure onset and offset times are calculated correctly
  print(paste("Date:", date, "Onset time:", onset_time, "Offset time:", offset_time))

  # Store the results for the current day
  results[[as.character(date)]] <- df_cage_day %>%
    mutate(onset_time = onset_time, offset_time = offset_time)
}

# Combine results into a single data frame for Cage 20
df_combined_cage <- bind_rows(results)

# Example: Plot for Cage 20 with grey background for lights off periods
ggplot(df_combined_cage, aes(x = timestamp, y = rolling_mean)) +
  geom_rect(data = df_combined_cage %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = start_time, xmax = as.POSIXct(paste(Date, "09:00"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = df_combined_cage %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = as.POSIXct(paste(Date, "21:00"), format="%Y-%m-%d %H:%M"), xmax = start_time + days(1), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_line() +
  geom_hline(data = df_combined_cage %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()

#####################

library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset time based on sustained activity above the mean for a given duration
find_onset_time <- function(df, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30) {
  duration_threshold <- sustained_minutes * 60

  # Onset: first time activity reaches above the mean and stays above for sustained duration before CoG
  df_before_cog <- df %>%
    filter(timestamp <= cog_time) %>%
    arrange(timestamp)

  cumulative_time_above_mean <- 0
  onset_time <- NA
  for (i in seq_len(nrow(df_before_cog))) {
    if (df_before_cog$rolling_mean[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + interval_minutes * 60
      if (cumulative_time_above_mean >= duration_threshold) {
        # Check if the activity falls back below the mean
        if (all(df_before_cog$rolling_mean[i:(i + duration_threshold / (interval_minutes * 60))] > daily_mean, na.rm = TRUE)) {
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

# Define a function to find offset time based on activity going below the mean for a given duration
find_offset_time <- function(df, daily_mean, interval_minutes = 10, sustained_minutes = 10) {
  duration_threshold <- sustained_minutes * 60

  # Offset: last time activity goes below the mean and stays below for sustained duration
  df_end_of_day <- df %>%
    arrange(desc(timestamp))

  cumulative_time_below_mean <- 0
  offset_time <- NA
  for (i in seq_len(nrow(df_end_of_day))) {
    if (df_end_of_day$rolling_mean[i] < daily_mean) {
      cumulative_time_below_mean <- cumulative_time_below_mean + interval_minutes * 60
      if (cumulative_time_below_mean >= duration_threshold) {
        # Check if the activity remains below the mean
        if (all(df_end_of_day$rolling_mean[i:(i + duration_threshold / (interval_minutes * 60))] < daily_mean, na.rm = TRUE)) {
          offset_time <- df_end_of_day$timestamp[i]
          break
        }
      }
    } else {
      cumulative_time_below_mean <- 0
    }
  }
  return(offset_time)
}

# Initialize an empty list to store results for Cage 20
results <- list()

# Filter data for Cage 20
df_cage <- df %>%
  filter(Cage == 20) %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Checkpoint 1: Ensure df_cage contains data for Cage 20
print(head(df_cage))

# Loop through each unique date within Cage 20
for (date in unique(df_cage$Date)) {
  # Filter data for the current date
  df_cage_day <- df_cage %>%
    filter(Date == date)

  # Calculate the rolling mean with a window size of 10
  df_cage_day <- df_cage_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 10, fill = 0, align = "center"))

  # Calculate the daily mean activity
  daily_mean <- df_cage_day %>%
    filter(timestamp >= as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p") &
             timestamp <= as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p")) %>%
    summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE)) %>%
    pull(mean_activity)
  df_cage_day$daily_mean <- daily_mean

  # Checkpoint 2: Ensure daily_mean is calculated correctly
  print(paste("Date:", date, "Daily Mean:", daily_mean))

  # Calculate Center of Gravity (CoG)
  cog_time <- df_cage_day %>%
    filter(!is.na(rolling_mean)) %>%
    summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
    pull(cog)
  cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

  # Checkpoint 3: Ensure CoG time is calculated correctly
  print(paste("Date:", date, "CoG time:", cog_time))

  # Find the onset and offset times
  onset_time <- find_onset_time(df_cage_day, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30)
  offset_time <- find_offset_time(df_cage_day, daily_mean, interval_minutes = 10, sustained_minutes = 10)

  # Checkpoint 4: Ensure onset and offset times are calculated correctly
  print(paste("Date:", date, "Onset time:", onset_time, "Offset time:", offset_time))

  # Store the results for the current day
  results[[as.character(date)]] <- df_cage_day %>%
    mutate(onset_time = onset_time, offset_time = offset_time)
}

# Combine results into a single data frame for Cage 20
df_combined_cage <- bind_rows(results)

# Example: Plot for Cage 20 with grey background for lights off periods
ggplot(df_combined_cage, aes(x = timestamp, y = rolling_mean)) +
  geom_rect(data = df_combined_cage %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = start_time, xmax = as.POSIXct(paste(Date, "09:00"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = df_combined_cage %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = as.POSIXct(paste(Date, "21:00"), format="%Y-%m-%d %H:%M"), xmax = start_time + days(1), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_line() +
  geom_hline(data = df_combined_cage %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()



###############


library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset time based on sustained activity above the mean for a given duration
find_onset_time <- function(df, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30) {
  duration_threshold <- sustained_minutes * 60

  # Onset: first time activity reaches above the mean and stays above for sustained duration before CoG
  df_before_cog <- df %>%
    filter(timestamp <= cog_time) %>%
    arrange(timestamp)

  cumulative_time_above_mean <- 0
  onset_time <- NA
  for (i in seq_len(nrow(df_before_cog))) {
    if (df_before_cog$rolling_mean[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + interval_minutes * 60
      if (cumulative_time_above_mean >= duration_threshold) {
        # Check if the activity falls back below the mean
        if (all(df_before_cog$rolling_mean[i:(i + duration_threshold / (interval_minutes * 60))] > daily_mean, na.rm = TRUE)) {
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

# Define a function to find offset time based on activity going below the mean for a given duration
find_offset_time <- function(df, daily_mean, interval_minutes = 10, sustained_minutes = 30) {
  duration_threshold <- sustained_minutes * 60

  # Offset: start from the end of the day and move backward to find the time when activity falls below the mean for the required duration
  df_end_of_day <- df %>%
    arrange(desc(timestamp))

  cumulative_time_below_mean <- 0
  offset_time <- NA
  for (i in seq_len(nrow(df_end_of_day))) {
    if (df_end_of_day$rolling_mean[i] < daily_mean) {
      cumulative_time_below_mean <- cumulative_time_below_mean + interval_minutes * 60
      if (cumulative_time_below_mean >= duration_threshold) {
        offset_time <- df_end_of_day$timestamp[i]
        break
      }
    } else {
      cumulative_time_below_mean <- 0
    }
  }
  return(offset_time)
}

# Initialize an empty list to store results for Cage 20
results <- list()

# Filter data for Cage 20
df_cage <- df %>%
  filter(Cage == 2) %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Checkpoint 1: Ensure df_cage contains data for Cage 20
print(head(df_cage))

# Loop through each unique date within Cage 20
for (date in unique(df_cage$Date)) {
  # Filter data for the current date
  df_cage_day <- df_cage %>%
    filter(Date == date)

  # Calculate the rolling mean with a window size of 10
  df_cage_day <- df_cage_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 10, fill = 0, align = "center"))

  # Calculate the daily mean activity
  daily_mean <- df_cage_day %>%
    filter(timestamp >= as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p") &
             timestamp <= as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p")) %>%
    summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE)) %>%
    pull(mean_activity)
  df_cage_day$daily_mean <- daily_mean

  # Checkpoint 2: Ensure daily_mean is calculated correctly
  print(paste("Date:", date, "Daily Mean:", daily_mean))

  # Calculate Center of Gravity (CoG)
  cog_time <- df_cage_day %>%
    filter(!is.na(rolling_mean)) %>%
    summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
    pull(cog)
  cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

  # Checkpoint 3: Ensure CoG time is calculated correctly
  print(paste("Date:", date, "CoG time:", cog_time))

  # Find the onset and offset times
  onset_time <- find_onset_time(df_cage_day, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30)
  offset_time <- find_offset_time(df_cage_day, daily_mean, interval_minutes = 10, sustained_minutes = 30)

  # Checkpoint 4: Ensure onset and offset times are calculated correctly
  print(paste("Date:", date, "Onset time:", onset_time, "Offset time:", offset_time))

  # Store the results for the current day
  results[[as.character(date)]] <- df_cage_day %>%
    mutate(onset_time = onset_time, offset_time = offset_time)
}

# Combine results into a single data frame for Cage 20
df_combined_cage <- bind_rows(results)

# Example: Plot for Cage 20 with grey background for lights off periods
ggplot(df_combined_cage, aes(x = timestamp, y = rolling_mean)) +
  geom_rect(data = df_combined_cage %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = start_time, xmax = as.POSIXct(paste(Date, "09:00"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = df_combined_cage %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = as.POSIXct(paste(Date, "21:00"), format="%Y-%m-%d %H:%M"), xmax = start_time + days(1), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_line() +
  geom_hline(data = df_combined_cage %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()


###################

library(zoo)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define a function to find onset time based on sustained activity above the mean for a given duration
find_onset_time <- function(df, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30) {
  duration_threshold <- sustained_minutes * 60

  # Onset: first time activity reaches above the mean and stays above for sustained duration before CoG
  df_before_cog <- df %>%
    filter(timestamp <= cog_time) %>%
    arrange(timestamp)

  cumulative_time_above_mean <- 0
  onset_time <- NA
  for (i in seq_len(nrow(df_before_cog))) {
    if (df_before_cog$rolling_mean[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + interval_minutes * 60
      if (cumulative_time_above_mean >= duration_threshold) {
        # Check if the activity falls back below the mean
        if (all(df_before_cog$rolling_mean[i:(i + duration_threshold / (interval_minutes * 60))] > daily_mean, na.rm = TRUE)) {
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

# Define a function to find offset time as the last time the activity falls below the mean
find_offset_time <- function(df, daily_mean) {
  # Offset: find the last time the activity falls below the mean
  df_below_mean <- df %>%
    filter(rolling_mean < daily_mean) %>%
    arrange(desc(timestamp))

  offset_time <- if (nrow(df_below_mean) > 0) {
    df_below_mean$timestamp[1]
  } else {
    NA
  }

  # Debug prints
  print("Offset calculation:")
  print(head(df_below_mean))
  print(paste("Offset time:", offset_time))

  return(offset_time)
}

# Initialize an empty list to store results for Cage 20
results <- list()

# Filter data for Cage 20
df_cage <- df %>%
  filter(Cage == 20) %>%
  mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

# Checkpoint 1: Ensure df_cage contains data for Cage 20
print(head(df_cage))

# Loop through each unique date within Cage 20
for (date in unique(df_cage$Date)) {
  # Filter data for the current date
  df_cage_day <- df_cage %>%
    filter(Date == date)

  # Calculate the rolling mean with a window size of 10
  df_cage_day <- df_cage_day %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 10, fill = 0, align = "center"))

  # Calculate the daily mean activity
  daily_mean <- df_cage_day %>%
    filter(timestamp >= as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p") &
             timestamp <= as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p")) %>%
    summarise(mean_activity = mean(HopsPerMinute, na.rm = TRUE)) %>%
    pull(mean_activity)
  df_cage_day$daily_mean <- daily_mean

  # Checkpoint 2: Ensure daily_mean is calculated correctly
  print(paste("Date:", date, "Daily Mean:", daily_mean))

  # Calculate Center of Gravity (CoG)
  cog_time <- df_cage_day %>%
    filter(!is.na(rolling_mean)) %>%
    summarise(cog = sum(rolling_mean * as.numeric(timestamp)) / sum(rolling_mean)) %>%
    pull(cog)
  cog_time <- as.POSIXct(cog_time, origin = "1970-01-01")

  # Checkpoint 3: Ensure CoG time is calculated correctly
  print(paste("Date:", date, "CoG time:", cog_time))

  # Find the onset and offset times
  onset_time <- find_onset_time(df_cage_day, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30)
  offset_time <- find_offset_time(df_cage_day, daily_mean)

  # Checkpoint 4: Ensure onset and offset times are calculated correctly
  print(paste("Date:", date, "Onset time:", onset_time, "Offset time:", offset_time))

  # Store the results for the current day
  results[[as.character(date)]] <- df_cage_day %>%
    mutate(onset_time = onset_time, offset_time = offset_time)
}

# Combine results into a single data frame for Cage 20
df_combined_cage <- bind_rows(results)

# Example: Plot for Cage 20 with grey background for lights off periods
ggplot(df_combined_cage, aes(x = timestamp, y = rolling_mean)) +
  geom_rect(data = df_combined_cage %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = start_time, xmax = as.POSIXct(paste(Date, "09:00"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = df_combined_cage %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = as.POSIXct(paste(Date, "21:00"), format="%Y-%m-%d %H:%M"), xmax = start_time + days(1), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_line() +
  geom_hline(data = df_combined_cage %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()



########


# Assuming df is your original dataframe
library(dplyr)

# Filter data for Cage 20 on 2023-03-07
data <- df %>%
  filter(Cage == 2 & Date == "2023-03-07")

library(zoo)

# Add a rolling mean column
data <- data %>%
  arrange(timestamp) %>%
  mutate(rolling_mean = rollmean(HopsPerMinute, k = 10, fill = NA, align = "right"))

daily_mean <- mean(data$HopsPerMinute, na.rm = TRUE)
data <- data %>%
  mutate(daily_mean = daily_mean)



# Identify onset and offset times
onset_time <- data %>%
  filter(rolling_mean > daily_mean) %>%
  slice(1) %>%
  pull(timestamp)

offset_time <- data %>%
  filter(rolling_mean > daily_mean) %>%
  slice_tail(n = 1) %>%
  pull(timestamp)


# Print results
print(paste("Daily Mean:", daily_mean))
print(paste("Onset Time:", onset_time))
print(paste("Offset Time:", offset_time))


###########


library(dplyr)
library(zoo)

# Filter data for Cage 20
cage_20_data <- df %>%
  filter(Cage == 20)

calculate_metrics <- function(data) {
  # Calculate the rolling mean with a 10-minute window
  data <- data %>%
    arrange(timestamp) %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 10, fill = NA, align = "right"))

  # Calculate the daily mean
  daily_mean <- mean(data$HopsPerMinute, na.rm = TRUE)
  data <- data %>%
    mutate(daily_mean = daily_mean)

  # Identify onset and offset times
  onset_time <- data %>%
    filter(rolling_mean > daily_mean) %>%
    slice(1) %>%
    pull(timestamp)

  offset_time <- data %>%
    filter(rolling_mean > daily_mean) %>%
    slice_tail(n = 1) %>%
    pull(timestamp)

  return(list(daily_mean = daily_mean, onset_time = onset_time, offset_time = offset_time))
}


# Get the unique dates for Cage 20
unique_dates <- unique(cage_20_data$Date)

# Initialize a list to store the results
results <- list()

# Loop through each date
for (date in unique_dates) {
  # Filter data for the current date
  daily_data <- cage_20_data %>%
    filter(Date == date)

  # Calculate metrics for the current date
  metrics <- calculate_metrics(daily_data)

  # Store the results
  results[[as.character(date)]] <- metrics
}


# Print results for each day
for (date in names(results)) {
  print(paste("Date:", date))
  print(paste("Daily Mean:", results[[date]]$daily_mean))
  print(paste("Onset Time:", results[[date]]$onset_time))
  print(paste("Offset Time:", results[[date]]$offset_time))
}

########

# Calculate metrics for each day
calculate_metrics <- function(data) {
  data <- data %>%
    arrange(timestamp) %>%
    mutate(rolling_mean = rollmean(HopsPerMinute, k = 10, fill = NA, align = "right"))

  daily_mean <- mean(data$HopsPerMinute, na.rm = TRUE)
  onset_time <- data %>%
    filter(rolling_mean > daily_mean) %>%
    slice(1) %>%
    pull(timestamp)

  offset_time <- data %>%
    filter(rolling_mean > daily_mean) %>%
    slice_tail(n = 1) %>%
    pull(timestamp)

  data <- data %>%
    mutate(daily_mean = daily_mean,
           onset_time = onset_time,
           offset_time = offset_time)

  return(data)
}

# Filter data for Cage 20
cage_20_data <- df %>%
  filter(Cage == 4)

# Get unique dates for Cage 20
unique_dates <- unique(cage_20_data$Date)

# Initialize a list to store results
results <- list()

# Loop through each date to calculate metrics
for (date in unique_dates) {
  daily_data <- cage_20_data %>%
    filter(Date == date)

  metrics <- calculate_metrics(daily_data)
  results[[as.character(date)]] <- metrics
}

# Combine results into a single data frame
combined_data <- bind_rows(results)


# Plotting
ggplot(combined_data, aes(x = timestamp, y = rolling_mean)) +
  geom_rect(data = combined_data %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = start_time, xmax = as.POSIXct(paste(Date, "09:00"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = combined_data %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = as.POSIXct(paste(Date, "21:00"), format="%Y-%m-%d %H:%M"), xmax = start_time + days(1), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_line() +
  geom_hline(data = combined_data %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()


# Extract onset and offset times for each day
onset_offset_times <- combined_data %>%
  select(Date, onset_time, offset_time) %>%
  distinct() %>%
  arrange(Date)

# Print the onset and offset times
print(onset_offset_times)


######################

library(dplyr)
library(zoo)
library(ggplot2)

# Calculate daily mean, onset time with rolling mean of 10, and offset time with rolling mean of 60
calculate_metrics <- function(data) {
  data <- data %>%
    arrange(timestamp) %>%
    mutate(rolling_mean_10 = rollmean(HopsPerMinute, k = 10, fill = NA, align = "right"),
           rolling_mean_60 = rollmean(HopsPerMinute, k = 30, fill = NA, align = "right"))

  daily_mean <- mean(data$HopsPerMinute, na.rm = TRUE)

  onset_time <- data %>%
    filter(rolling_mean_10 > daily_mean) %>%
    slice(1) %>%
    pull(timestamp)

  offset_time <- data %>%
    filter(rolling_mean_60 > daily_mean) %>%
    slice_tail(n = 1) %>%
    pull(timestamp)

  data <- data %>%
    mutate(daily_mean = daily_mean,
           onset_time = onset_time,
           offset_time = offset_time)

  return(data)
}

# Filter data for Cage 20
cage_20_data <- df %>%
  filter(Cage == 2)

# Get unique dates for Cage 20
unique_dates <- unique(cage_20_data$Date)

# Initialize a list to store results
results <- list()

# Loop through each date to calculate metrics
for (date in unique_dates) {
  daily_data <- cage_20_data %>%
    filter(Date == date)

  metrics <- calculate_metrics(daily_data)
  results[[as.character(date)]] <- metrics
}

# Combine results into a single data frame
combined_data <- bind_rows(results)

# Extract onset and offset times for each day
onset_offset_times <- combined_data %>%
  select(Date, onset_time, offset_time) %>%
  distinct() %>%
  arrange(Date)

# Print the onset and offset times
print(onset_offset_times)

# Plotting
ggplot(combined_data, aes(x = timestamp, y = rolling_mean_10)) +
  geom_rect(data = combined_data %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = start_time, xmax = as.POSIXct(paste(Date, "09:00"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = combined_data %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = as.POSIXct(paste(Date, "21:00"), format="%Y-%m-%d %H:%M"), xmax = start_time + days(1), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_line(aes(y = rolling_mean_10), color = "black") +
  geom_hline(data = combined_data %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 1) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 1) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()


##################


library(dplyr)
library(zoo)
library(ggplot2)

# Define the function to find onset time based on sustained activity above the mean for a given duration
find_onset_time <- function(df, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30) {
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
    if (!is.na(df_before_cog$rolling_mean_10[i]) && df_before_cog$rolling_mean_10[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + interval_minutes * 60
      if (cumulative_time_above_mean >= duration_threshold) {
        # Check if the activity falls back below the mean
        if (all(df_before_cog$rolling_mean_10[i:(i + duration_threshold / (interval_minutes * 60))] > daily_mean, na.rm = TRUE)) {
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

# Calculate daily mean, onset time with custom function, and offset time with rolling mean of 30
calculate_metrics <- function(data) {
  data <- data %>%
    arrange(timestamp) %>%
    mutate(rolling_mean_10 = rollmean(HopsPerMinute, k = 10, fill = NA, align = "right"),
           rolling_mean_30 = rollmean(HopsPerMinute, k = 30, fill = NA, align = "right"))

  daily_mean <- mean(data$HopsPerMinute, na.rm = TRUE)
  cog_time <- as.POSIXct(data$timestamp[floor(nrow(data)/2)], origin="1970-01-01") # Example CoG time

  onset_time <- find_onset_time(data, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30)

  offset_time <- data %>%
    filter(rolling_mean_30 > daily_mean) %>%
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

# Filter data for Cage 20
cage_20_data <- df %>%
  filter(Cage == 20)

# Get unique dates for Cage 20
unique_dates <- unique(cage_20_data$Date)

# Initialize a list to store results
results <- list()

# Loop through each date to calculate metrics
for (date in unique_dates) {
  daily_data <- cage_20_data %>%
    filter(Date == date)

  if (nrow(daily_data) > 0) {
    metrics <- calculate_metrics(daily_data)
    results[[as.character(date)]] <- metrics
  }
}

# Combine results into a single data frame
combined_data <- bind_rows(results)

# Extract onset and offset times for each day
onset_offset_times <- combined_data %>%
  select(Date, onset_time, offset_time) %>%
  distinct() %>%
  arrange(Date)

# Print the onset and offset times
print(onset_offset_times)

# Plotting
ggplot(combined_data, aes(x = timestamp, y = rolling_mean_10)) +
  geom_rect(data = combined_data %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = start_time, xmax = as.POSIXct(paste(Date, "09:00"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = combined_data %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
            aes(xmin = as.POSIXct(paste(Date, "21:00"), format="%Y-%m-%d %H:%M"), xmax = start_time + days(1), ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) +
  geom_line(aes(y = rolling_mean_10), color = "black") +
  geom_hline(data = combined_data %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 1) +
  geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 1) +
  facet_wrap(~ Date, scales = "free_x", ncol = 1) +
  labs(title = "Rolling Average and Onset/Offset Relative to CoG for Cage 20", x = "Time", y = "Rolling Average") +
  theme_classic()

########################


library(dplyr)
library(zoo)
library(ggplot2)

# Define the function to find onset time based on sustained activity above the mean for a given duration
find_onset_time <- function(df, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30) {
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
    if (!is.na(df_before_cog$rolling_mean_10[i]) && df_before_cog$rolling_mean_10[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + interval_minutes * 60
      if (cumulative_time_above_mean >= duration_threshold) {
        # Check if the activity falls back below the mean
        if (all(df_before_cog$rolling_mean_10[i:(i + duration_threshold / (interval_minutes * 60))] > daily_mean, na.rm = TRUE)) {
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

# Calculate daily mean, onset time with custom function, and offset time with rolling mean of 30
calculate_metrics <- function(data) {
  data <- data %>%
    arrange(timestamp) %>%
    mutate(rolling_mean_10 = rollmean(HopsPerMinute, k = 10, fill = NA, align = "right"),
           rolling_mean_30 = rollmean(HopsPerMinute, k = 30, fill = NA, align = "right"))

  daily_mean <- mean(data$HopsPerMinute, na.rm = TRUE)
  cog_time <- as.POSIXct(data$timestamp[floor(nrow(data)/2)], origin="1970-01-01") # Example CoG time

  onset_time <- find_onset_time(data, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30)

  offset_time <- data %>%
    filter(rolling_mean_30 > daily_mean) %>%
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

# Filter data for all cages
unique_cages <- unique(df$Cage)

# Initialize a list to store results
results <- list()

# Loop through each cage and date to calculate metrics
for (cage in unique_cages) {
  cage_data <- df %>%
    filter(Cage == cage)

  unique_dates <- unique(cage_data$Date)

  for (date in unique_dates) {
    daily_data <- cage_data %>%
      filter(Date == date)

    if (nrow(daily_data) > 0) {
      metrics <- calculate_metrics(daily_data)
      results[[paste(cage, date, sep = "_")]] <- metrics
    }
  }
}

# Combine results into a single data frame
combined_data <- bind_rows(results)

# Function to extract and print onset and offset times for a specific cage
extract_times <- function(cage_number) {
  on_off_times <- combined_data %>%
    filter(Cage == cage_number) %>%
    select(Cage, Date, onset_time, offset_time) %>%
    distinct() %>%
    arrange(Date)

  print(on_off_times)
}

# Example: Extract and print onset and offset times for Cage 20
extract_times(2)

#Plot
plot_cage_data <- function(cage_number) {
  cage_data <- combined_data %>%
    filter(Cage == cage_number)

  ggplot(cage_data, aes(x = timestamp, y = rolling_mean_10)) +
    geom_rect(data = cage_data %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
              aes(xmin = start_time, xmax = as.POSIXct(paste(Date, "09:00"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_rect(data = cage_data %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
              aes(xmin = as.POSIXct(paste(Date, "21:00"), format="%Y-%m-%d %H:%M"), xmax = start_time + days(1), ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_line(aes(y = rolling_mean_10), color = "black") +
    geom_hline(data = cage_data %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = as.numeric(onset_time)), linetype = "dotted", color = "blue", size = 2) +
    geom_vline(aes(xintercept = as.numeric(offset_time)), linetype = "dotted", color = "blue", size = 2) +
    facet_wrap(~ Date, scales = "free_x", ncol = 1) +
    labs(title = paste("Rolling Average and Onset/Offset Relative to CoG for Cage", cage_number), x = "Time", y = "Rolling Average") +
    theme_classic()
}

# Example: Plot data for Cage 20
plot_cage_data(2)



####################


# Function to plot data for a specific cage
plot_cage_data <- function(cage_number) {
  cage_data <- combined_data %>%
    filter(Cage == cage_number)

  ggplot(cage_data, aes(x = timestamp, y = rolling_mean_10)) +
    geom_rect(data = cage_data %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
              aes(xmin = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M"), xmax = as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_rect(data = cage_data %>% mutate(start_time = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M")),
              aes(xmin = as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %H:%M"), xmax = as.POSIXct(paste(Date, "24:00"), format="%Y-%m-%d %H:%M"),
                                                                                     ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_line(aes(y = rolling_mean_10), color = "black") +
    geom_hline(data = cage_data %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
    geom_point(aes(x = onset_time, y = 0), color = "blue", size = 2) +
    geom_point(aes(x = offset_time, y = 0), color = "blue", size = 2) +
    facet_wrap(~ Date, scales = "free_x", ncol = 1) +
    labs(title = paste("Rolling Average and Onset/Offset Relative to CoG for Cage", cage_number), x = "Time", y = "Rolling Average") +
    theme_classic()
}

# Example: Plot data for Cage 20
plot_cage_data(20)


# Function to plot data for a specific cage
plot_cage_data <- function(cage_number) {
  cage_data <- combined_data %>%
    filter(Cage == cage_number)

  ggplot(cage_data, aes(x = timestamp, y = rolling_mean_10)) +
    geom_rect(data = cage_data,
              aes(xmin = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M"),
                  xmax = as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p"), ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_rect(data = cage_data,
              aes(xmin = as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p"),
                  xmax = as.POSIXct(paste(Date, "23:59"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_line(aes(y = rolling_mean_10), color = "black") +
    geom_hline(data = cage_data %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
    geom_point(aes(x = onset_time, y = 0), color = "blue", size = 2) +
    geom_point(aes(x = offset_time, y = 0), color = "blue", size = 2) +
    facet_wrap(~ Date, scales = "free_x", ncol = 1) +
    labs(title = paste("Rolling Average and Onset/Offset Relative to CoG for Cage", cage_number), x = "Time", y = "Rolling Average") +
    theme_classic()
}

# Example: Plot data for Cage 20
plot_cage_data(20)


###################

find_onset <- function(df, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30) {
  duration_threshold <- sustained_minutes * 60

  df <- df %>%
    arrange(timestamp) %>%
    mutate(rolling_mean_10 = rollmean(HopsPerMinute, k = 10, fill = NA, align = "right"))

  # Onset: first time activity reaches above the mean and stays above for sustained duration before CoG
  df_before_cog <- df %>%
    filter(timestamp <= cog_time) %>%
    arrange(timestamp)

  if (nrow(df_before_cog) == 0) {
    return(list(onset_time = NA, rolling_mean_10 = df$rolling_mean_10))
  }

  cumulative_time_above_mean <- 0
  onset_time <- NA
  for (i in seq_len(nrow(df_before_cog))) {
    if (!is.na(df_before_cog$rolling_mean_10[i]) && df_before_cog$rolling_mean_10[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + interval_minutes * 60
      if (cumulative_time_above_mean >= duration_threshold) {
        # Check if the activity falls back below the mean
        if (all(df_before_cog$rolling_mean_10[i:(i + duration_threshold / (interval_minutes * 60))] > daily_mean, na.rm = TRUE)) {
          onset_time <- df_before_cog$timestamp[i]
          break
        }
      }
    } else {
      cumulative_time_above_mean <- 0
    }
  }
  return(list(onset_time = onset_time, rolling_mean_10 = df$rolling_mean_10))
}


activity_times <- function(df) {
  unique_cages <- unique(df$Cage)
  results <- list()

  for (cage in unique_cages) {
    cage_data <- df %>%
      filter(Cage == cage)

    unique_dates <- unique(cage_data$Date)

    for (date in unique_dates) {
      daily_data <- cage_data %>%
        filter(Date == date)

      if (nrow(daily_data) > 0) {
        daily_mean <- mean(daily_data$HopsPerMinute, na.rm = TRUE)
        cog_time <- as.POSIXct(daily_data$timestamp[floor(nrow(daily_data) / 2)], origin="1970-01-01") # Example CoG time

        onset_result <- find_onset(daily_data, cog_time, daily_mean, interval_minutes = 10, sustained_minutes = 30)
        onset_time <- onset_result$onset_time
        daily_data$rolling_mean_10 <- onset_result$rolling_mean_10

        daily_data <- daily_data %>%
          mutate(onset_time = onset_time)

        daily_data <- find_offset(daily_data)
        results[[paste(cage, date, sep = "_")]] <- daily_data
      }
    }
  }

  combined_data <- bind_rows(results)
  return(combined_data)
}



extract_times <- function(combined_data, cage_number) {
  on_off_times <- combined_data %>%
    filter(Cage == cage_number) %>%
    select(Cage, Date, onset_time, offset_time) %>%
    distinct() %>%
    arrange(Date)

  print(on_off_times)
}

# Assuming 'df' is your data frame with the required columns
rolling_onset <- 10
rolling_offset <- 30

combined_data <- activity_times(df, rolling_onset, rolling_offset)

# Extract and print onset and offset times for Cage 20
extract_times(combined_data, 20)

##################


# Load necessary libraries
library(dplyr)
library(zoo)

# # Define the find_onset function
# find_onset <- function(df, cog_time, daily_mean, rolling_onset = 10, interval_minutes = 10, sustained_minutes = 30) {
#   duration_threshold <- sustained_minutes * 60
#
#   df <- df %>%
#     arrange(timestamp) %>%
#     mutate(rolling_mean_onset = rollmean(HopsPerMinute, k = rolling_onset, fill = NA, align = "right"))
#
#   # Onset: first time activity reaches above the mean and stays above for sustained duration before CoG
#   df_before_cog <- df %>%
#     filter(timestamp <= cog_time) %>%
#     arrange(timestamp)
#
#   if (nrow(df_before_cog) == 0) {
#     return(list(onset_time = NA, rolling_mean_onset = df$rolling_mean_onset))
#   }
#
#   cumulative_time_above_mean <- 0
#   onset_time <- NA
#   for (i in seq_len(nrow(df_before_cog))) {
#     if (!is.na(df_before_cog$rolling_mean_onset[i]) && df_before_cog$rolling_mean_onset[i] > daily_mean) {
#       cumulative_time_above_mean <- cumulative_time_above_mean + interval_minutes * 60
#       if (cumulative_time_above_mean >= duration_threshold) {
#         if (all(df_before_cog$rolling_mean_onset[i:(i + duration_threshold / (interval_minutes * 60))] > daily_mean, na.rm = TRUE)) {
#           onset_time <- df_before_cog$timestamp[i]
#           break
#         }
#       }
#     } else {
#       cumulative_time_above_mean <- 0
#     }
#   }
#   return(list(onset_time = onset_time, rolling_mean_onset = df$rolling_mean_onset))
# }

# Example data frame
df <- data.frame(
  timestamp = seq.POSIXt(from = as.POSIXct("2023-03-03 00:00"), to = as.POSIXct("2023-03-03 23:59"), by = "min"),
  HopsPerMinute = runif(1440, min = 0, max = 5)
)

# Calculate daily mean
daily_mean <- mean(df$HopsPerMinute, na.rm = TRUE)

# Calculate CoG time as the midpoint of the timestamps
cog_time <- df$timestamp[floor(nrow(df) / 2)]

# Use the find_onset function
onset_result <- find_onset(df, cog_time, daily_mean, rolling_onset = 10, sustained_minutes = 30)

# Print the results
print(onset_result$onset_time)
print(onset_result$rolling_mean_onset)


extract_onset_times <- function(df) {
  unique_dates <- unique(as.Date(df$timestamp))
  onset_times <- data.frame(Date = as.Date(character()), OnsetTime = as.POSIXct(character()))

  for (date in unique_dates) {
    daily_data <- df %>%
      filter(as.Date(timestamp) == date)

    daily_mean <- mean(daily_data$HopsPerMinute, na.rm = TRUE)
    cog_time <- daily_data$timestamp[floor(nrow(daily_data) / 2)]

    onset_result <- find_onset(daily_data, cog_time, daily_mean, rolling_onset = 10, sustained_minutes = 30)

    onset_times <- rbind(onset_times, data.frame(Date = date, OnsetTime = onset_result$onset_time))
  }

  return(onset_times)
}


# Extract onset times for each date
onset_times <- extract_onset_times(Cage_20)

# Print the results
print(onset_times)

##################

library(dplyr)
library(zoo)

# Define the function to find onset time based on sustained activity above the mean for a given duration
find_onset <- function(df, cog_time, daily_mean, rolling_onset = 10, sustained_minutes = 30) {
  duration_threshold <- sustained_minutes * 60

  df <- df %>%
    arrange(timestamp) %>%
    mutate(rolling_mean_onset = rollmean(HopsPerMinute, k = rolling_onset, fill = NA, align = "right"))

  # Onset: first time activity reaches above the mean and stays above for sustained duration before CoG
  df_before_cog <- df %>%
    filter(timestamp <= cog_time) %>%
    arrange(timestamp)

  if (nrow(df_before_cog) == 0) {
    return(list(onset_time = NA, rolling_mean_onset = df$rolling_mean_onset))
  }

  cumulative_time_above_mean <- 0
  onset_time <- NA
  for (i in seq_len(nrow(df_before_cog))) {
    if (!is.na(df_before_cog$rolling_mean_onset[i]) && df_before_cog$rolling_mean_onset[i] > daily_mean) {
      cumulative_time_above_mean <- cumulative_time_above_mean + rolling_onset * 60
      if (cumulative_time_above_mean >= duration_threshold) {
        # Check if the activity falls back below the mean
        if (all(df_before_cog$rolling_mean_onset[i:(i + duration_threshold / (rolling_onset * 60))] > daily_mean, na.rm = TRUE)) {
          onset_time <- df_before_cog$timestamp[i]
          break
        }
      }
    } else {
      cumulative_time_above_mean <- 0
    }
  }
  return(list(onset_time = onset_time, rolling_mean_onset = df$rolling_mean_onset))
}

# Extract onset times for each date
extract_onset_times <- function(df) {
  # Ensure the timestamp column is in POSIXct format
  df$timestamp <- as.POSIXct(df$timestamp, origin = "1970-01-01")

  # Explicitly extract the date from the timestamp
  df <- df %>%
    mutate(Date = as.Date(format(timestamp, "%Y-%m-%d")))

  unique_dates <- unique(df$Date)

  # Initialize the onset_times data frame
  onset_times <- data.frame(Date = as.Date(character()), OnsetTime = as.POSIXct(character()))

  for (date in unique_dates) {
    daily_data <- df %>%
      filter(Date == date)

    if (nrow(daily_data) > 0) {
      daily_mean <- mean(daily_data$HopsPerMinute, na.rm = TRUE)
      cog_time <- daily_data$timestamp[floor(nrow(daily_data) / 2)]

      onset_result <- find_onset(daily_data, cog_time, daily_mean, rolling_onset = 10, sustained_minutes = 30)

      onset_times <- rbind(onset_times, data.frame(Date = date, OnsetTime = onset_result$onset_time))
    }
  }

  return(onset_times)
}

# Test with example data frame
# Example data frame (replace with your actual data)
df <- data

# Extract onset times for each date
onset_times <- extract_onset_times(Cage_20)

# Print the results
print(onset_times)
