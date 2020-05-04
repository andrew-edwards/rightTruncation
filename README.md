rightTruncation

R package for maximum likelihood estimation of delay distributions of right-truncated data (as during COVID-19 pandemic)

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/andrew-edwards/rightTruncation.svg?branch=master)](https://travis-ci.org/andrew-edwards/rightTruncation)
[![Codecov test coverage](https://codecov.io/gh/andrew-edwards/rightTruncation/branch/master/graph/badge.svg)](https://codecov.io/gh/andrew-edwards/rightTruncation?branch=master)
<!-- badges: end -->

# Description

**Under development** - the main code is written, just converting it into a package.

For an epidemiological modelling study of the COVID-19 pandemic, we have dates on which new confirmed cases are reported in British Columbia, Canada; full details are given in our manuscript at https://www.medrxiv.org/content/10.1101/2020.04.17.20070086v1. For some of these case we also have dates of estimated onset of symptoms. These data are used to parameterise a Weibull distribution of the delay between symptom onset and a case being reported. The number of newly-symptomatic people each day is calculated in the epidemiological model (twelve coupled differential equations), and then, via the delay distribution and an observation model, compared to the number of reported cases at later dates. 

Since we are modelling the epidemic *during* the epidemic, our data are right-truncated. Consider someone who became symptomatic on 17 April 2020, got tested and this resulted in a positive result that is reported on 26 April. So there's a 9-day delay in symptom onset to reporting. But if someone else became symptomatic on 25 April and was also going to have a 9-day delay until reporting, if today is 28 April then we do not know that person is going to show up in our data (because they haven't gone for testing yet). That is right-truncation, and needs to be taken into account when fitting the Weibull distribution. The likelihood function to do this is derived in the manuscript; this package contains functions for implementation. Similar applications likely occur in ecological contexts.
