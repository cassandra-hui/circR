#' Plot Data for a Specific Cage
#'
#' This function plots the rolling mean activity data for a specific cage, with grey background indicating lights off periods.
#'
#' @param data A data frame containing activity data with columns 'Cage', 'timestamp', 'rolling_mean_onset', 'Date', 'LightsOn', 'LightsOff', 'daily_mean', 'onset_time', and 'offset_time'.
#' @param cage_number An integer specifying the cage number to plot.
#' @return A ggplot object showing the rolling mean activity for the specified cage, with onset and offset times indicated.
#' @import dplyr
#' @import ggplot2
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_classic
#' @importFrom magrittr %>%
#'
#' @examples
#' # Assuming combined_data is your data frame with the required columns
#' plot_data(combined_data, 20)
#' @export
plot_data <- function(data, cage_number) {
  cage_data <- data %>%
    filter(Cage == cage_number)

  ggplot(cage_data, aes(x = timestamp, y = rolling_mean_onset)) +
    geom_rect(data = cage_data,
              aes(xmin = as.POSIXct(paste(Date, "00:00"), format="%Y-%m-%d %H:%M"),
                  xmax = as.POSIXct(paste(Date, LightsOn), format="%Y-%m-%d %I:%M %p"), ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_rect(data = cage_data,
              aes(xmin = as.POSIXct(paste(Date, LightsOff), format="%Y-%m-%d %I:%M %p"),
                  xmax = as.POSIXct(paste(Date, "23:59"), format="%Y-%m-%d %H:%M"), ymin = -Inf, ymax = Inf),
              fill = "grey", alpha = 0.5) +
    geom_line(aes(y = rolling_mean_onset), color = "black") +
    geom_hline(data = cage_data %>% distinct(Date, daily_mean), aes(yintercept = daily_mean), linetype = "dashed", color = "red") +
    geom_point(aes(x = onset_time, y = 0), color = "blue", size = 2) +
    geom_point(aes(x = offset_time, y = 0), color = "blue", size = 2) +
    facet_wrap(~ Date, scales = "free_x", ncol = 1) +
    labs(title = paste("Rolling Average and Onset/Offset Relative to CoG for Cage", cage_number), x = "Time", y = "Rolling Average") +
    theme_classic()
}
