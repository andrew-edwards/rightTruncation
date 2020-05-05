

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
##'   (both dates) and `time_to_report` (numeric), such as `delay-data`
##' @param day_0 date to consider as day 0. If NULL then uses
##'   `min(input$symptom_onset_date)`
##' @param day_N date to consider as day N. If NULL then uses `max(input$reported_date)`
##' @return tibble with columns `n`, `r` and `h_nr`
##' @export
##' @author Andrew Edwards
make_h_nr_tibble <- function(input,
                         day_0 = NULL,
                         day_N = NULL){
  if(is.null(day_0)) day_0 <- min(input$symptom_onset_date)
  if(is.null(day_N)) day_N <- max(input$reported_date)

  delay_1 = input %>%
    dplyr::group_by(reported_date, symptom_onset_date) %>%
    dplyr::summarise(n()) %>%
    dplyr::rename("h_nr" = "n()") %>%
    dplyr::filter(symptom_onset_date >= day_0 & reported_date <= day_N) %>%
    dplyr::mutate(reported_day = as.numeric(reported_date - day_0),
                  symptom_onset_day = as.numeric(symptom_onset_date - day_0))

  h_nr_tibble <- dplyr::select(dplyr::ungroup(delay_1),
                               n = symptom_onset_day,
                               r = reported_day,
                               h_nr = h_nr) %>%
    dplyr::arrange(n, r)

  h_nr_tibble
}
