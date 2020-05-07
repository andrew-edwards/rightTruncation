##' functions-from-funcs.R - copied over from
##' /CoronaModelsBC/selfIsolationModel/SIR-functionalised/funcs.R, then moving
##' one at a time into new files as proper functions. This file is in .Rbuildignore.




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
