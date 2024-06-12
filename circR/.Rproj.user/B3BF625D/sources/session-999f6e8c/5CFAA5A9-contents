plot_cage_data <- function(combined_data, cage_number) {
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
