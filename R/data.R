##' Raw data from British Columbia on time of symptom onset and time of
##' reporting, with random noise added since data cannot be public
##'
##' From the linelist data, see `data-raw/symptom-to-report.R` for details.
##' @format A tibble with each row being a confirmed case (not all confirmed
##'   cases are here, only those with estimated date of symptom onset), and columns:
##'   * reported_date - date of case being reported (lubridate date format)
##'   * symptom_onset_date - date of symptom onset of that case
##'   * time_to_report - difference of first two columns, in days
##' @source Generated from running `data-raw/symptom-to-report.R`.
"delay_data"
