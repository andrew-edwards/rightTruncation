##' Plot time from onset of symptoms to case being reported.
##'
##' Bar plots for each day, plus points for each case. Adapted from Mike
##'  Irvine's code, could replace with Sean's later code that was used in the
##'  manuscript. Need bit more description.
##'
##'
##' @param data tibble with columns XX
##' @param start_date_report First date of reported case to show.
##' @param start_date_onset First date of symptom onset to show
##' @param show_weekends Highlight weekends or not
##' @param x_axis Either `reported` to show date of case being reported, or
##'   `onset` for date of symptom onset
##' @param xLim Limits of dates for x-axis
##' @param yLab Label for y-axis
##' @return
##' @export
##' @author Andrew Edwards adapted from Mike Irvine's code
plot_time_to_report <- function(data,
                                start_date_report = "2020-03-10",
                                start_date_onset = "2020-03-05",
                                show_weekends = FALSE,
                                x_axis = "reported",
                                xLim = c(lubridate::ymd("2020-02-28"), lubridate::ymd("2020-04-07")),
                                yLab = "Time from symptom onset\n to reported case (days)"
                                ){
  stopifnot(x_axis %in% c("reported", "onset"))

  if(x_axis == "reported"){
    p <- data %>% dplyr::filter(reported_date >= start_date_report) %>%
      dplyr::mutate(weekday = dplyr::if_else(lubridate::wday(reported_date) %in%
                                      c(7,1),
                                      "Weekend",
                                      "Weekday")
                    ) %>%
      ggplot2::ggplot(ggplot2::aes(x = reported_date,
                                   y = time_to_report)) +
      ggplot2::geom_boxplot(ggplot2::aes(group=reported_date),
                            outlier.shape = NA,
                            lwd = 0.3,
                            alpha = 0.2) +
      ggplot2::xlim(xLim) +
      ggplot2::geom_jitter(height = 0,
                           size = 0.4) +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::labs(x = "Date of reported case",
                    y = yLab)
    if(show_weekends){ p <- p + ggplot2::geom_boxplot(ggplot2::aes(group =
                                                                     reported_date,
                                                                   fill = weekday))}
  }

  if(x_axis == "onset"){
    p <- data %>%
      dplyr::filter(symptom_onset_date >= start_date_onset) %>%
      dplyr::mutate(weekday = dplyr::if_else(lubridate::wday(reported_date) %in%
                                      c(7,1),
                                      "Weekend",
                                      "Weekday")
                    ) %>%
      ggplot2::ggplot(ggplot2::aes(x = symptom_onset_date,
                                   y = time_to_report)) +
      ggplot2::geom_boxplot(ggplot2::aes(group=symptom_onset_date),
                            outlier.shape = NA,
                            lwd = 0.3) +
      ggplot2::xlim(xLim) +
      ggplot2::geom_jitter(height = 0,
                           size = 0.4) +
      ggplot2::geom_abline(slope = -1,
                           intercept = lubridate::ymd(max(data$reported_date)),
                           linetype = "dashed") +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::labs(x = "Date of onset of symptoms",
                    y = yLab)
    if(show_weekends){ p <- p + ggplot2::geom_boxplot(ggplot2::aes(
                                                                 group = symptom_onset_date,
                                                                 fill = weekday))}
  }
  p
}
