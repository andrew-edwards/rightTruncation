##' likelihood.R - functions for likelihood calculations

##' Calculate negative log-likelihood for the Weibull distribution given count data
##'  that are right-truncated, with data as a matrix
##'
##' Calculate the negative log-likelihood of the parameters `k` and `lambda`
##' given count data. Returns the negative log-likelihood. The data are in the
##' form of a square matrix of dimension (N+1)x(N+1).
##' Will be called by `nlm()` or similar. This is more for explanation purposes
##' -- use `negLL_Weibull_counts_df()` for real data since it should be faster
##' and is set up to do confidence intervals.
##'
##' @param p vector of parameter values `c(k, lambda)`, where `k` is the shape
##'   parameter and `lambda` is the scale parameter, for which to calculate the
##'   negative log-likelihood
##' @param h_nr square matrix of counts of numbers of individuals whose case was
##'   reported on day `r` and who first reported symptoms on day `n`. `r` and `n` start
##'   from 0 so `h_nr[1, 1]` is $h_{0, 0}$ etc., i.e.  `h_nr[n+1, r+1]` is $h_{nr}$ in the
##'   mathematical derivations. `dim(h_nr)` is (N+1)x(N+1)
##' @return negative log-likelihood of the parameters given the data
##' @export
##' @author Andrew Edwards
negLL_Weibull_counts_matrix = function(p, h_nr){
    k = p[1]
    lambda = p[2]
    Nplus1 = nrow(h_nr)                 # N+1 since row 1 is n=0
    loglike_nr = matrix(NA,
                        nrow = Nplus1,
                        ncol = Nplus1)  # each component of the log-likelihood:
    sumlogLL = 0
    for(i in 1:Nplus1){
      n = i - 1                  # i = 1, n = 0
      for(j in i:Nplus1){        # should be n+1 columns
        r = j - 1                # j = i = 1, n = 0 and r = 0;
        pweibull_diff = pweibull(r - n + 1,
                                 shape = k,
                                 scale = lambda) -
          pweibull(r - n,
                   shape = k,
                   scale = lambda)
        # component of log likelihood, avoiding log(0) or log(NaN):
        if(is.na(pweibull_diff)) pweibull_diff = 0
        loglike_nr[i, j] = ifelse(pweibull_diff > 0,
                                  h_nr[i, j] * ( log(pweibull_diff) -
                                                 log(pweibull(r + 1,
                                                              shape = k,
                                                              scale = lambda))),
                                  0)
        sumlogLL = sumlogLL + loglike_nr[i, j]
      }
    }
    #if(sumlogLL == 0) {sumlogLL = NA}
    neglogLL = - sumlogLL
    if(neglogLL < 1e-6) neglogLL = 10e10   # avoid NA's
    return(neglogLL)
}

##' Wrapper function for simulating one h_nr matrix and fitting with likelihood
##' using `nlm()`
##'
##' @param init initial conditions for optimisation
##' @param ... inputs for `h_nr_simulate()`
##' @return MLE_res output from nlm, a list containing components:
##'  - `minimum`: minimum of negative log-likelihod
##'  - `estimate`: vector of MLEs of `k` and then `lambda`
##'  - `gradient`: vector of the gradient at the minimum (should be close to `c(0,0)`)
##'  - `code`: integer indicating why the optimization process terminated, 1 is
##'   relative gradient is close to zero (see `?nlm`).
##'  - `iterations`: number of iterations performed
##' @export
##' @author Andrew Edwards
h_nr_one_sim_fit <- function(init = c(3, 15),
                             ...){
  h_nr_sim <- h_nr_simulate(...)
  MLE_res = nlm(f = negLL_Weibull_counts_matrix,
                p = init,
                h_nr = h_nr_sim)
  return(MLE_res)
}

##' Calculate negative log-likelihood for the Weibull distribution given count data
##' that are right-truncated and are in a long tibble format
##'
##' Calculate the negative log-likelihood of the parameters `k` and `lambda`
##' given count data. Returns the negative log-likelihood.
##' Will be called by `nlm()` or similar. Data are in a data frame tibble format rather
##' than matrix format (as in `negLL_Weibull_counts_matrix()`) since the matrix
##' will always be upper triangular (so using a long data frame should be
##' faster), and data frame is easier to obtain from real data. This is also set up
##' to do confidence intervals (unlike `negLL_Weibull_counts_matrix()`).
##'
##' @param p vector of parameter values `c(k, lambda)` [shape and scale,
##'   respectively] for which to calculate the
##'   negative log-likelihood, or just one or the other if `k_MLE` or `lambda_MLE`
##'   are specified (for univariate confidence intervals).
##' @param h_nr_df tibble of counts of numbers of individuals whose case was
##'   reported on day `r` and who first reported symptoms on day `n`. `r` and `n` start
##'   from 0. Dataframe has columns `n`, `r` and `h_nr`, which is more concise than
##'   the matrix formulation with lots of 0's. So $h_{ab}$ in the math is
##'   `dplyr::filter(h_nr_df, n = a, r = b)$h_nr`  (the pairwise combinations of n and r
##'   are unique).
##' @param k_MLE fixed value of `k_MLE` to use for calculating univariate
##'   confidence interval of `lambda` (needed since `profLike()` is univariate)
##' @param lambda_MLE fixed value of `lambda_MLE` to use for calculating univariate
##'   confidence interval of `k`.
##' @return negative log-likelihood of the parameters given the data
##' @export
##' @author Andrew Edwards
negLL_Weibull_counts_df = function(p,
                                   h_nr_df,
                                   k_MLE = NULL,
                                   lambda_MLE = NULL){
  if(length(p) == 1){      # Fix one at MLE for confidence interval calculation
    if(is.null(k_MLE)){
      k = p
      lambda = lambda_MLE}
    if(is.null(lambda_MLE)){
      k = k_MLE
      lambda = p}
  } else {
    k = p[1]
    lambda = p[2]
  }

  # Can probably simplify this since now using data frame.
  N = max(h_nr_df$r)
    sumlogLL = 0
    for(n_val in 0:N){            # nval in loop
      for(r_val in n_val:N){
        h_nr_val <- dplyr::filter(h_nr_df,
                                  n == n_val & r == r_val) %>%
          dplyr::pull(h_nr)
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
          sumlogLL = sumlogLL + loglike_inc
          loglike_inc = 0
        }

      }
    }
    #if(sumlogLL == 0) {sumlogLL = NA}
    neglogLL = - sumlogLL
    if(neglogLL < 1e-6) neglogLL = 10e10   # avoid NA's
    return(neglogLL)
}
