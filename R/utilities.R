

##' Make a tibble of h_nr values from a tibble of symptom onset and reporting
##' dates
##'
##' Given a tibble (such as `delay_data`) with columns `reported_date`,
##'  `symptom_onset_date` (both dates) and `time_to_report` (numeric), create
##'  a tibble with columns `n` (symptom onset day), `r` (day of reporting) and
##'  `h_nr` (number of cases whose symptoms started on day `n` and case was
##'  reported on day `r`). Day start at 0, so $n, r \geq 0$. Do not need zero
##'  `h_nr` counts, so this is a bit easier to work with than the matrix form.
##' TODO Needs expanding for ICU and hospitalisation data.
##'
##' @param input tibble containing columns `reported_date`, `symptom_onset_date`
##'   (both dates) and `time_to_report` (numeric)
##' @return  tibble with columns `n`, `r` and `h_nr`
##' @export
##' @author Andrew Edwards
make_h_nr_df <- function(input){

}
