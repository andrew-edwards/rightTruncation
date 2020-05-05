##' load-data.R - loading in and tidying up data


##' Load in the detailed linelist data and exclude un-needed columns
##'
##' Load in the detailed linelist data and exclude un-needed columns (that may
##'  want to be looked at some point) and outliers of negative delays and delays
##'  >30 days (since likely data-input errors). Data are not public (yet) and so
##'  this function will not work for everyone.
##'
##' @return list of two tibbles with columns `reported_date`, `symptom_onset_date and
##'   `time_to_report` with each row corresponding to a positive test:
##'  - delay_data_with_outliers  -- with the outliers still included
##'  - delay_data                -- with outliers excluded, to use for analyses
##' @export
##' @author Andrew Edwards
load_tidy_delay_data <- function(){
  linelist_latest_file <-
    here::here("../CoronaModelsBC/nCoVDailyData/linelist/2019-nCoV_daily_linelist.csv")
  if(!file.exists(linelist_latest_file)){
    stop(paste("You need to have the file ",
               linelist_latest_file,
               " ."))
  }
  linelist <- read.csv(linelist_latest_file,
                     stringsAsFactors = F,
                     na.strings = "")
  names(linelist)[1] = "investigation_id"    # else it seems to be "i..investigation_id"

  delay_data_with_outliers = dplyr::as_tibble(linelist) %>%
    dplyr::select(reported_date, symptom_onset_date) %>%
    dplyr::mutate_all(lubridate::ymd) %>%
    dplyr::filter(!is.na(reported_date) & !is.na(symptom_onset_date)) %>%
    dplyr::mutate(time_to_report = reported_date - symptom_onset_date)

  # Removing the following outliers that are $<0$ or $\geq 30$ days, since most
  #  likely to be data-entry errors, to yield the final dataset.
  # filter(delay_data_with_outliers,
  #     time_to_report >= 30 | time_to_report < 0)
  delay_data <- dplyr::filter(delay_data_with_outliers,
                              time_to_report < 30 & time_to_report >= 0)
  return(list("delay_data" = delay_data,
              "delay_data_with_outliers" = delay_data_with_outliers))
}
