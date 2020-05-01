##' functions-from-funcs.R - copied over from
##' /CoronaModelsBC/selfIsolationModel/SIR-functionalised/funcs.R, then moving
##' one at a time into new files as proper functions. This file is in .Rbuildignore.

##' Load in the detailed linelist data and exclude un-needed columns (that may
##'  want to be looked at some point) and outliers of negative delays and delays
##'  >30 days (since likely data-input errors).
##'  <description>
##'
##' @return list of two tibbles with columns `reported_date`, `symptom_onset_date and
##'   `time_to_report` with each row corresponding to a positive test:
##'  - delay_data_with_outliers  - with the outliers still included
##'  - delay_data                - with outliers excluded, to use for analyses
##' @export
##' @author Andrew Edwards
load_tidy_delay_data <- function(){
  linelist_latest_file <-
    here::here("nCoVDailyData/linelist/2019-nCoV_daily_linelist.csv")
  if(!file.exists(linelist_latest_file)){
    stop(paste("You need to have the file ",
               linelist_latest_file,
               " ."))
  }
  linelist <- read.csv(linelist_latest_file,
                     stringsAsFactors = F,
                     na.strings = "")
  names(linelist)[1] = "investigation_id"    # else it seems to be "i..investigation_id"

  delay_data_with_outliers = as_tibble(linelist) %>%
    select(reported_date, symptom_onset_date) %>%
    mutate_all(lubridate::ymd) %>%
    filter(!is.na(reported_date) & !is.na(symptom_onset_date)) %>%
    mutate(time_to_report = reported_date - symptom_onset_date)

  # Removing the following outliers that are $<0$ or $\geq 30$ days, since most
  #  likely to be data-entry errors, to yield the final dataset.
  # filter(delay_data_with_outliers,
  #     time_to_report >= 30 | time_to_report < 0)
  delay_data <- filter(delay_data_with_outliers,
                       time_to_report < 30 & time_to_report >= 0)
  return(list("delay_data" = delay_data,
              "delay_data_with_outliers" = delay_data_with_outliers))
}



##' Calculate negative log-likelihood for the Weibull distribution given count data
##'   that are right-truncated, but in a data frame format not a
##'   matrix, since real data give a sparse matrix.
##'
##' Calculate the negative log-likelihood of the parameters `k` and `lambda`
##' given count data. Returns the negative log-likelihood.
##' Will be called by `nlm()` or similar.
##'
##' @param p vector of parameter values c(k, lambda) for which to calculate the
##'   negative log-likelihood, or just one or the other if k_MLE or lambda_MLE
##'   are specified (for univariate confidence intervals).
##' @param h_nr_df dataframe of counts of numbers of individuals whose case was
##'   reported on day r and who first reported symptoms on day n. r and n start
##'   from 0. Dataframe has columns n, r and h_nr, which is more concise than
##'   the matrix formulation with lots of 0's. So h_{ab} in the math is
##'   filter(h_nr_df, n = a, r = b)$h_nr  (the pairwise combinations of n and r
##'   are unique).
##' @param k_MLE fixed value of k_MLE to use for calculating univariate
##'   confidence interval of lambda (needed since profLike is univariate)
##' @param lambda_MLE fixed value of lambda_MLE to use for calculating univariate
##'   confidence interval of k.
##' @return negative log-likelihood of the parameters given the data
##' @export
##' @author Andrew Edwards
negLL.Weibull.counts.df = function(p, h_nr_df, k_MLE = NULL, lambda_MLE = NULL){
  if(length(p) == 1){      # Fix one at MLE for confidence interval calculation
    if(is.null(k_MLE)) {
      k = p
      lambda = lambda_MLE}
    if(is.null(lambda_MLE)){
      k = k_MLE
      lambda = p}
  } else {
    k = p[1]
    lambda = p[2]
  }

  N = max(h_nr_df$r)
    sumlogLL = 0
    for(n_val in 0:N){            # nval in loop
      for(r_val in n_val:N){
        h_nr_val <- dplyr::filter(h_nr_df, n == n_val & r == r_val) %>%
          pull(h_nr)
        if(!(length(h_nr_val) == 0)){       # have data so need calculations
          if(length(h_nr_val) > 1){
            stop("h_nr_df should have unique combinations of r and n")}  # move
                                        # to test
                                        # before calling, then not in every
                                        # loglike call.
          pweibull_diff = pweibull(r_val - n_val + 1,
                                 shape = k,
                                 scale = lambda) -
            pweibull(r_val - n_val,
                     shape = k,
                     scale = lambda)
          # component of log likelihood, avoiding log(0) or log(NaN):
          if(is.na(pweibull_diff)) { pweibull_diff = 0}
          loglike_inc = ifelse(pweibull_diff > 0,
                               h_nr_val * ( log(pweibull_diff) -
                                            log(pweibull(r_val + 1,
                                                         shape = k,
                                                         scale = lambda))),
                               0)
          # if(is.na(loglike_nr[i,j])) browser() # stop("stopping")
          # print(loglike_nr[i,j])
          sumlogLL = sumlogLL + loglike_inc
          loglike_inc = 0
        }

      }
    }
    #if(sumlogLL == 0) {sumlogLL = NA}
    neglogLL = - sumlogLL
    if(neglogLL < 1e-6) neglogLL = 10e10   # avoid NA's
    # browser()
    return(neglogLL)
}



##' Simulate h_nr data and plot likelihood surface
##'
##' @return
##' @export
##' @author Andrew Edwards
negLL_Weibull_likelihood_plot <- function(){
  h_nr_sim <- h_nr_simulate()
             # try ... again
  mygrid = as.matrix(expand.grid(k = seq(from = 1,
                                         to = 3.0,
                                         length = 11),
                                 lambda = seq(from = 1,
                                              to = 15)))
  res = apply(mygrid,
              1,
              function(x) negLL.Weibull.counts(p = c(x[1], x[2]),
                                               h_nr = h_nr_sim))

  mygrid <- as.data.frame(mygrid)
  mygrid['Likelihood'] <- res

  ggplot(data = mygrid,
         aes(x = k,
             y = lambda,
             fill = res)) +
   geom_tile() # +
  # viridis::scale_fill_viridis()
}


# Profile log-likelihood method to calculate 95\% confidence interval.
#  Taken directly from
# https://github.com/andrew-edwards/sizeSpectra/blob/master/R/likelihood.R#L65
#
##' Profile log-likelihood method to calculate 95\% confidence interval
##'  for a given negative log-likelihood function, maximum likelihood estimate
##'  (MLE) and minimum of of the negative log-likelihood function. Based on
##'  Hilborn and Mangel (1997), The Ecological Detective, p162. Only for a
##'  single parameter.
##'
##' @param negLL.fn negative log-likelihood function that take arguments
##'   (parameters and data) in ... and returns a negative log-likelihood value
##' @param MLE maximum likelihood estimate (already calculated)
##' @param minNegLL the minimum of the negative log-likelihood function, at the
##'   MLE (by definition)
##' @param vecDiff value defining  range over which to test the negative log-likelihood
##'   to construct the confidence interval; range is `MLE` \eqn{\pm} `vecDiff`. Default is 0.5 and a symmetric
##'   range is tested for fitting size spectra, since for movement data
##'   sets in Table 2 of Edwards (2011; 92(6):1247-1257) the intervals were
##'   symmetric, so symmetric seems a good start.
##' @param vecInc increments to try, the accuracy of the resulting bounds
##'      will depend on this. Note that a resulting interval of, say,
##'     (-2.123, -1.987) means that that interval is contained within the
##'     true 95\% interval, which is itself contained within (-2.124, -1.986).
##'     The true bounds lie between the stated lower bounds and between
##'     the stated upper bounds. So reduce vecInc if further accuracy is needed.
##' @param ... further arguments to `negLL.fn()`
##' @return two-component vector of the 95\% confidence interval
##' @export
##' @author Andrew Edwards
profLike = function(negLL.fn, MLE, minNegLL, vecDiff=0.5, vecInc=0.001, ...)
    {
    vec = seq(MLE - vecDiff, MLE + vecDiff, vecInc)
                 # Values of parameter to test to obtain confidence interval

    # LLvals = vector(length=length(bvec))
    LLvals = sapply(X=vec, FUN=negLL.fn, ...)
    critVal = minNegLL  + qchisq(0.95,1)/2
                      # 1 degree of freedom, Hilborn and Mangel (1997) p162.
    vecIn95 = vec[ LLvals < critVal ]
                      # values in 95% confidence interval
    conf = c(min(vecIn95), max(vecIn95))
    if(conf[1] == min(vec) | conf[2] == max(vec))
      { dev.new()
        plot(vec, LLvals)
        abline(h = critVal, col="red")
        stop("Need to make vecDiff larger - see R window")   # Could automate
      }
    return(conf)
}
