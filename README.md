# Using the circR Package for Data Analysis
This guide provides step-by-step instructions on how to use the provided R functions for circadain data analysis. The functions are designed to help analyze automated perch data from the Ouyang Lab but can work for other data sources.

## Table of Contents
- [Prerequisites](#prerequisites)
- [Installing Required Packages](#installing-required-packages)
- [Finding On- and Offset Times](#finding-on--and-offset-times)
- [Extracting Times](#extracting-times)
- [Plotting Data](#plotting-data)

## Prerequisites

Before you begin, make sure you have the following prerequisites:
- Properly formatted data in a data frame (Use the Ouyang package or see sample layout).

## Installing Required Packages

The provided functions use several R packages. To install them and this package, execute the following code in R:

```r
install.packages(c("dplyr", "zoo", "ggplot2", "lubridate"))
devtools::install_github("cassandra-hui/circR")
library(circR)
```

## Finding On- and Offset Times

### Activty Onset Calculation
1.) Rolling Mean Calculation:
  - A rolling mean is calculated over a specified window (rolling_onset defualt set to 10 miuntes) to smooth the activity data.
2.) Daily Mean Calculation:
  - The daily mean of bird activity is calculated to set a threshold for sustained activity.
3.) Activity Before CoG (Center of Gravity) Time:
  - The data is filtered to include only the activity before the CoG time.
4.) Sustained Activity Check:
  - The onset time is identified as the first instance when the rolling mean stays above the daily mean for a specified duration (sustained_minutes defualt set to 30 minutes).

### Activity Offset Calculation
1.) Rolling Mean Calculation:
- A rolling mean is calculated over a different specified window (rolling_offset default set to 30 minutes) to smooth the activity data. This rolling mean can be set differently from the onset rolling mean which may be useful based on sepcies. Since we work with birds and their activity becomes more speratict in the evening we set a higher rolling mean value to filter out short bursts of activity at night.

2.) Daily Mean Calculation:
- The daily mean of bird activity is used as a threshold for identifying the end of sustained activity.

3.) Sustained Inactivity Check:
- The offset time is identified as the last instance when the rolling mean stays above the daily mean before falling below it for the sustained duration.

### Function: activity_times

This function analyzes your data frame to find onset and offset of activity relative to center point of activity. 

```r

#Analze activity onset and offset times
#Select onset and offset rolling means to smooth activity
onset_roll <- 10
offset_roll <- 30

#Run function with selected parameters
data <- activity_times(df, onset_roll = onset_roll, offset_roll = offset_roll)


```


## Extracting Times

### Function: extracting_times

This function extracts the onset and offset times by cage (or ID) and prints them out.

```r
# Extract and print onset and offset times for specified cage 
extract_times(data, 20) #Times selected for cage 20

```

## Plotting Data

### Function: plot_data

A simple funcation can be run to view activity with onset and offset times maked by blue dots. 

```r
# Plot data for Cage 20
plot_data(data, 20)
```

---

By following these instructions, you can effectively use the provided R functions to analyze your activity data.
