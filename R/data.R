##' Raw data from British Columbia on time of symptom onset and time of
##' reporting.
##'
##' From the linelist data, see `data-raw/symptom-to-report.R` for details. This
##'   was done in June 2020 and has a latest `reported_date` of 2020-06-02. Used
##'   to explain methods in the vignette.
##' @format A tibble with each row being a confirmed case (not all confirmed
##'   cases are here, only those with estimated date of symptom onset), and columns:
##'   * reported_date - date of case being reported (lubridate date format)
##'   * symptom_onset_date - date of symptom onset of that case
##'   * time_to_report - difference of first two columns, in days
##' @source Generated from running `data-raw/symptom-to-report.R`.
"delay_data"

##' From the linelist data, see `data-raw/symptom-to-report.R` for details.
##' @format Same as `delay_data`, but done for data uploaded on 23 September 2020
##'   (so max `reported_date` is 22nd). Due presumably to slight delays in
##'   amalgamating data, `dplyr::filter(delay_data_2020_09_22, reported_date <=
##'   "2020-06-02")` has five more rows than `delay_data` (which has
##'   `max(reported_date)` of 2020-06-02. Hence worth saving a few example data
##'   sets from various dates.
"delay_data_2020_09_22"

##' From the linelist data, see `data-raw/symptom-to-report.R` for details.
##' @format Same as `delay_data` and `delay_data_2020_09_22`, but done for data
##'   uploaded on 2021-01-11 (with max `reported_date` of 2021-01-05).
"delay_data_2020_01_05"
