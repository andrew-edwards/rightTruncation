# rightTruncation

R package for maximum likelihood estimation of delay distributions of right-truncated data (as during COVID-19 pandemic)

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/andrew-edwards/rightTruncation.svg?branch=master)](https://travis-ci.com/andrew-edwards/rightTruncation)
<!-- [![Codecov test coverage](https://codecov.io/gh/andrew-edwards/rightTruncation/branch/master/graph/badge.svg)](https://codecov.io/gh/andrew-edwards/rightTruncation?branch=master) -->
<!-- badges: end -->

# Description

This package was developed for our paper:

**Quantifying the impact of COVID-19 control measures using a Bayesian model of physical distancing** by
Sean C. Anderson, Andrew M. Edwards, Madi Yerlanov, Nicola Mulberry, Jessica E. Stockdale, Sarafa A. Iyaniwura, Rebeca C. Falcao, Michael C. Otterstatter, Michael A. Irvine, Naveed Z. Janjua, Daniel Coombs, and Caroline Colijn. ***PLOS Computational Biology***. 2020, 16(12): e1008274. Freely avaiable at <https://doi.org/10.1371/journal.pcbi.1008274>.

For an epidemiological modelling study of the COVID-19 pandemic, we have dates on which new confirmed cases are reported in British Columbia, Canada. For some of these case we also have dates of estimated onset of symptoms. These data are used to parameterise a Weibull distribution of the delay between symptom onset and a case being reported. The number of newly-symptomatic people each day is calculated in the epidemiological model (twelve coupled differential equations), and then, via the delay distribution and an observation model, compared to the number of reported cases at later dates. That is done in the package [*covidseir*](https://github.com/seananderson/covidseir). 

Since we are modelling the epidemic *during* the epidemic, our data are right-truncated. Consider someone who became symptomatic on 17 April 2020, got tested and this resulted in a positive result that is reported on 26 April. So there's a 9-day delay in symptom onset to reporting. But if someone else became symptomatic on 25 April and was also going to have a 9-day delay until reporting, if today is 28 April then we do not know that person is going to show up in our data (because they haven't gone for testing yet). That is right-truncation, and needs to be taken into account when fitting the Weibull distribution. The likelihood function to do this is derived in the manuscript; this package contains functions for implementation. Similar applications likely occur in ecological contexts.

The vignette can be viewed [directly here](http://htmlpreview.github.io/?https://github.com/andrew-edwards/rightTruncation/blob/master/doc/rightTruncation.html).

# Installation instructions

To install this package directly from GitHub you need the package `devtools`, so install that if you do not have it (once):

```
install.packages("devtools")
```

To install the latest version of `rightTruncation`:

```
devtools::install_github("andrew-edwards/rightTruncation")
```

If you get asked to update other packages, try saying 'none of them' (I've been okay lately, and package works with R 4.0.0.0).

Then:
```
library(rightTruncation)
vignette("rightTruncation")
```
to run the vignette, or see it in the link above.

<!-- [Note that if you are using Rstudio there is a [known issue](https://github.com/rstudio/rstudio/issues/2253) that equations don't render properly in the viewer; they are fine in a usual html viewer. Also `browseVignettes("sizeSpectra")` didn't seem to work in Rstudio either. Running from a different R console (e.g. the default non-Rstudio version) should work fine.]-->

## Issues, problems

Please report any problems as a [GitHub Issue](https://github.com/andrew-edwards/rightTruncation/issues), using a minimal working example if possible (and please check the closed issues first).

