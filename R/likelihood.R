##' likelihood.R - functions for likelihood calculations

##' Calculate negative log-likelihood for the Weibull distribution given count data
##'  that are right-truncated, with data as a matrix
##'
##' Calculate the negative log-likelihood of the parameters `k` and `lambda`
##' given count data. Returns the negative log-likelihood. The data are in the
##' form of a square matrix of dimension (N+1)x(N+1).
##' Will be called by `nlm()` or similar.
##'
##' @param p vector of parameter values `c(k, lambda)`, where `k` is the shape
##'   parameter and `lambda` is the scale parameter, for which to calculate the
##'   negative log-likelihood
##' @param h_nr square matrix of counts of numbers of individuals whose case was
##'   reported on day r and who first reported symptoms on day n. r and n start
##'   from 0 so h_nr[1, 1] is h_{0, 0} etc.:  h_nr[n+1, r+1] = h_{nr} in the
##'   math. dim(h_nr) is (N+1)x(N+1)
##' @return negative log-likelihood of the parameters given the data
##' @export
##' @author Andrew Edwards
negLL.Weibull.counts.matrix = function(p, h_nr){
    k = p[1]
    lambda = p[2]
    Nplus1 = nrow(h_nr)                 # N+1 since row 1 is n=0
    loglike_nr = matrix(NA,
                        nrow = Nplus1,
                        ncol = Nplus1)  # each component of the
    # log-likelihood
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
##' @param num_cases how many to simulate
##' @param k shape parameter of Weibull delay function
##' @param lambda scale parameter of Weibull delay function
##' @param N maximum day of reporting, days start at 0
##' @param seed seed for random number generator
##' @return MLE.res output from nlm, a list containing components:
##'  - `minimum`: minimum of negative log-likelihod
##'  - `estimate`: vector of MLEs of `k` and then `lambda`
##'  - `gradient`: vector of the gradient at the minimum (should be close to `c(0,0)`)
##'  - `code`: integer indicating why the optimization process terminated, 1 is
##'   relative gradient is close to zero (see `?nlm`).
##'  - `iterations`: number of iterations performed
##' @export
##' @author Andrew Edwards
h_nr_one_sim_fit <- function(num_cases = 100, k = 2, lambda = 9, N = 30){
  h_nr_sim <- h_nr_simulate(num_cases = num_cases,
                            k = k,
                            lambda = lambda,
                            N = N)
  MLE.res = nlm(f = negLL.Weibull.counts.matrix,
                p = c(3, 15),
                h_nr = h_nr_sim)
  return(MLE.res)
}
