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

# Installation instructions

To install this package directly from GitHub you need the package `devtools`, so if you don't have it install it (once):

```
install.packages("devtools")
```

To install the latest version of `rightTruncation`:

```
devtools::install_github("andrew-edwards/rightTruncation")
```

If you get asked to update other packages, try replying to update none of them.

<!-- 
Then:
```
library(rightTruncation)
browseVignettes("rightTruncation")
```
to list the vignettes (and links to their Rmarkdown, R and html versions). `Vignettes_overview` gives an overview of all vignettes, and is available either from the previous command or by just running
```
vignette("Vignettes_overview")
```
[Note that if you are using Rstudio there is a [known issue](https://github.com/rstudio/rstudio/issues/2253) that equations don't render properly in the viewer; they are fine in a usual html viewer. Also `browseVignettes("sizeSpectra")` didn't seem to work in Rstudio either. Running from a different R console (e.g. the default non-Rstudio version) should work fine.]

## Issues, problems

Please report any problems as a [GitHub Issue](https://github.com/andrew-edwards/sizeSpectra/issues), using a minimal working example if possible (and please check the closed issues first).
-->