## As instructed from using usethis::use_tibble() [else saved data tibbles don't
##  get used properly]

## usethis namespace: start
#' @importFrom tibble tibble
#' @importFrom grDevices dev.new dev.off png
#' @importFrom graphics abline axis box hist legend lines matlines mtext par
#'   plot points rect segments strwidth text
#' @importFrom stats coef confint lm na.omit nlm predict qchisq quantile runif
#' @importFrom utils data read.csv
#' @importFrom magrittr "%>%"
#' @importFrom stats pweibull rweibull
#'
## usethis namespace: end
NULL

# As for gfiphc repo, need these to avoid warnings related to dplyr
# commands (e.g. referring to the column names withing dplyr::filter()).
# Copied from the warning given by check() (that puts them alphabetical, then
# had some more to add at the end).
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
           "h_nr",
           "n",
           "r",
           "reported_date",
           "reported_day",
           "symptom_onset_date",
           "symptom_onset_day",
           "time_to_report",
           "weekday"
  ))
}
