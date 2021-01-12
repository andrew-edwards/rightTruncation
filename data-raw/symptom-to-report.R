# Load in the symptoms to reporting data, and save it here.
#  (Originally from file report-delay.Rmd).

delay_data <- load_tidy_delay_data()[["delay_data"]]

usethis::use_data(delay_data,
                  overwrite = TRUE)
