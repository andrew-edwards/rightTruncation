# Load in the symptoms to reporting data, and save it here.
#  (Originally from file report-delay.Rmd).

# This was done in June 2020 and has max reported_date of 2020-06-02. Keeping as
#  default since useful for explanations in the vignette.
delay_data <- load_tidy_delay_data()[["delay_data"]]

usethis::use_data(delay_data,
                  overwrite = FALSE)   # No longer update this

# Doing this, which has a maximum reported date of 2020_09_22, to maybe add to
#  vignette.
delay_data_2020_09_22 <- load_tidy_delay_data()[["delay_data"]]

usethis::use_data(delay_data_2020_09_22,
                  overwrite = FALSE)   # No longer update this
#  delay_data is not quite the same as
#   dplyr::filter(delay_data_2020_09_22, reported_date <= "2020-06-02")
#  probably due to data still tricking in for delay_data. Hence worth saving
#   these at various times.


# Doing this on 2021-01-11, though max reported_date is 2021-01-05, to add to vignette and update
#  parameter estimates for modelling.
delay_data_2021_01_05 <- load_tidy_delay_data()[["delay_data"]]

usethis::use_data(delay_data_2021_01_05,
                  overwrite = FALSE)   # No longer update this
