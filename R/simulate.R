##' Simulation functions

##' Simulate numbers of cases with symptoms on day n that are reported on day r
##'
##' Returns a matrix (that's how I first did it), so modify at some point to
##'   return dataframe.
##' @param num_cases how many cases to simulate
##' @param k shape parameter of Weibull delay function
##' @param lambda scale parameter of Weibull delay function
##' @param N maximum day of reporting, days start at 0
##' @param seed seed for random number generator
##' @return h_nr matrix of number of cases, with h_nr[n+1, r+1] corresponding to
##'   h_nr (since n and r start at 0)
##' @export
##' @author Andrew Edwards
h_nr_simulate <- function(num_cases = 100,
                          k = 2,
                          lambda = 9,
                          N = 30,
                          seed = 42){
  set.seed(seed)
  day_reported <- vector()
  #  the r for an individual
  h_nr <-  matrix(NA, N + 1, N + 1)
                          # h_nr[n+1, r+1] is h_nr (n and r start from 0)
      for(npar in 0:N){   # loop over true days
        # May not need to save as matrix, just want h_nr.
        # floor here since day r is reports through that day (so by end of day)
        day_reported = floor(rweibull(num_cases,
                                      shape = k,
                                      scale = lambda)) + npar
        for(r in 0:N){
          h_nr[npar+1, r+1] <- sum(abs(day_reported - r) < 1e-6)  # counts on
                                             # day r from infection on day n
        }
      }
  return(h_nr)
}
