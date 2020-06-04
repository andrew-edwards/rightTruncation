## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  fig.width = 4,
  fig.height = 4,
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(rightTruncation)

## ----simulated----------------------------------------------------------------
h_nr_simulated <- h_nr_simulate(N = 10)
h_nr_simulated

## ----likematrix---------------------------------------------------------------
MLE_res = nlm(f = negLL_Weibull_counts_matrix,
              p = c(3, 15),
              h_nr = h_nr_simulated)

## ----res----------------------------------------------------------------------
k_MLE <- MLE_res$estimate[1]
lambda_MLE <- MLE_res$estimate[2]
k_MLE      # shape
lambda_MLE # scale
mean_using_MLEs <- lambda_MLE * gamma(1 + 1/k_MLE)
mean_using_MLEs
median_using_MLEs <- lambda_MLE * (log(2))^(1/k_MLE)
median_using_MLEs

## ----simest-------------------------------------------------------------------
h_nr_one_sim_fit(N=10)$estimate

## ---- delaydata---------------------------------------------------------------
delay_data
time_report_vec = 0:(as.numeric(max(delay_data$time_to_report)) + 2)
hist(as.numeric(delay_data$time_to_report),
     breaks = time_report_vec,
     right = FALSE,
     xlab = "Time from symptom onset to reported case (days)",
     main = "",
     col = "lightgrey")

## ---- cases-------------------------------------------------------------------
plotdelay1 <- plot_time_to_report(delay_data, x_axis = "onset",
                                  xLim = c(lubridate::ymd("2020-02-28"),
                                           max(delay_data$reported_date) + 2))
plotdelay2 <- plot_time_to_report(delay_data,
                                  xLim = c(lubridate::ymd("2020-02-28"),
                                           max(delay_data$reported_date) + 2))

## ---- plots, fig.width = 8, fig.asp = 0.8-------------------------------------
gridExtra::grid.arrange(
             plotdelay1,
             plotdelay2,
             ncol=1)

## ----convert------------------------------------------------------------------
h_nr_tibble <- make_h_nr_tibble(delay_data)
h_nr_tibble
# Show how many of each h_nr values there are:
summary(as.factor(h_nr_tibble$h_nr))

## ----MLEtibble----------------------------------------------------------------
MLE <- nlm(f = negLL_Weibull_counts_tibble,
           p = c(3,15),
           h_nr_tibble = h_nr_tibble)

k_MLE <- MLE$estimate[1]
lambda_MLE <- MLE$estimate[2]
k_MLE      # shape
lambda_MLE # scale
mean_using_MLEs <- lambda_MLE * gamma(1 + 1/k_MLE)
mean_using_MLEs
median_using_MLEs <- lambda_MLE * (log(2))^(1/k_MLE)
median_using_MLEs

## ----manuscript---------------------------------------------------------------
h_nr_manuscript <- make_h_nr_tibble(delay_data,
                                    day_0 = lubridate::ymd("2020-02-29"),
                                    day_N = lubridate::ymd("2020-04-02"))
MLE_man <- nlm(f = negLL_Weibull_counts_tibble,
               p = c(3,15),
               h_nr_tibble = h_nr_manuscript)
k_MLE_man <- MLE_man$estimate[1]
lambda_MLE_man <- MLE_man$estimate[2]
k_MLE_man      # shape
lambda_MLE_man # scale
mean_using_MLEs_man <- lambda_MLE_man * gamma(1 + 1/k_MLE_man)
mean_using_MLEs_man
median_using_MLEs_man <- lambda_MLE_man * (log(2))^(1/k_MLE_man)
median_using_MLEs_man

## ----MLEhist, eval=FALSE------------------------------------------------------
#  png(filename = "delay-hist-BC-NZ.png",
#      width = 6.8*240,
#      height = 4.5*240,
#      res = 240)
#  max_delay <- max(c(h_nr_manuscript$r - h_nr_manuscript$n, h_NZ$breaks) + 2)
#  time_report_vec = seq(0, max_delay, by=0.1)
#  
#  h_NZ <- readRDS(paste0(here::here(), "/data-raw/NZ_histogram.rds"))
#  
#  time_report_BC = dweibull(time_report_vec,
#                            shape = k_MLE_man,
#                            scale = lambda_MLE_man) * sum(h_nr_manuscript$h_nr)
#  time_report_NZ = dweibull(time_report_vec,
#                            shape = 1.53289,
#                            scale = 7.818021) * sum(h_NZ$counts)
#  par(mfrow=c(1,2))
#  hist(rep(h_nr_manuscript$r - h_nr_manuscript$n, h_nr_manuscript$h_nr),
#       breaks = 0:max_delay,
#       right = FALSE,
#       xlab = "Days from symptom onset to reporting",
#       main = "British Columbia",
#       col = "lightgrey")
#  lines(time_report_vec,
#        time_report_BC,
#        col = "red",
#        lwd = 2)
#  plot(h_NZ,
#       xlab = "Days from symptom onset to reporting",
#       main = "New Zealand",
#       col = "lightgrey")
#  lines(time_report_vec,
#        time_report_NZ,
#        col = "red",
#        lwd = 2)
#  dev.off()

## ----conf, eval=FALSE---------------------------------------------------------
#  k_confint <- profLike(negLL.Weibull.counts.df,
#                        MLE = k_MLE,
#                        minNegLL = MLE.res$minimum,
#                        vecInc = 0.001,
#                        h_nr_df = h_nr_df,
#                        lambda_MLE = lambda_MLE)
#  k_confint  #  1.603928 1.857928   = 1.60-1.86
#  
#  lambda_confint <- profLike(negLL.Weibull.counts.df,
#                        MLE = lambda_MLE,
#                        minNegLL = MLE.res$minimum,
#                        vecInc = 0.001,
#                        vecDiff = 0.65,
#                        h_nr_df = h_nr_df,
#                        k_MLE = k_MLE)
#  lambda_confint  #  9.304579 10.462579   = 9.30-10.46
#  # THEN check nlm works okay still

## ----NZ1----------------------------------------------------------------------
delay_data

## ----NZ2----------------------------------------------------------------------
h_nr_tibble <- make_h_nr_tibble(delay_data)
h_nr_tibble

## ----NZ3----------------------------------------------------------------------
MLE <- nlm(f = negLL_Weibull_counts_tibble,
           p = c(3,15),
           h_nr_tibble = h_nr_tibble)
MLE

