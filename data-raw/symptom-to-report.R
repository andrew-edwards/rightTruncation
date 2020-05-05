# Load in the symptoms to reporting data, and save it randomised here since not
# yet public. This is from report-delay.Rmd. If can make public the true data
# then can comment out all except first and last lines.

delay_data <- load_tidy_delay_data()[["delay_data"]]

# Add randomness to be able to make public
add_random_1 <- sample(-3:3, nrow(delay_data), replace=T)
add_random_2 <- sample(-3:3, nrow(delay_data), replace=T)

delay_data <- delay_data %>%
  dplyr::mutate(reported_date = reported_date + add_random_1) %>%
  dplyr::mutate(symptom_onset_date = symptom_onset_date + add_random_2) %>%
  dplyr::mutate(time_to_report = as.numeric(reported_date - symptom_onset_date)) %>%
  dplyr::filter(time_to_report >= 0)

usethis::use_data(delay_data,
                  overwrite = TRUE)
