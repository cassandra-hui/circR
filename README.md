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
