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

If you get asked to update other packages, try saying 'none of them' (I've been okay lately).

Then:
```
library(rightTruncation)
vignette("rightTruncation")
```
to run the vignette.

[Note that if you are using Rstudio there is a [known issue](https://github.com/rstudio/rstudio/issues/2253) that equations don't render properly in the viewer; they are fine in a usual html viewer. Also `browseVignettes("sizeSpectra")` didn't seem to work in Rstudio either. Running from a different R console (e.g. the default non-Rstudio version) should work fine.]

## Issues, problems

Please report any problems as a [GitHub Issue](https://github.com/andrew-edwards/rightTruncation/issues), using a minimal working example if possible (and please check the closed issues first).

# Quick run for New Zealand data

We would like to fit a Weibull distribution to data on the delays between a person having the onset of symptoms and them being reported as a confirmed cases of COVID-19. The data we have in British Columbia is a tibble with these (self-explanatory) headings (final column is just difference between the first two): 

```
> delay_data
 
 A tibble: 1,066 x 3

   reported_date symptom_onset_date time_to_report

   <date>        <date>             <drtn>        
 1 2020-01-27    2020-01-22          5 days       
 2 2020-02-02    2020-01-27          6 days       
 3 2020-02-05    2020-01-24         12 days       
 4 2020-02-05    2020-01-15         21 days       
 5 2020-02-11    2020-02-09          2 days       
 6 2020-02-20    2020-02-14          6 days       
 7 2020-02-22    2020-02-18          4 days       
 8 2020-03-02    2020-02-25          6 days       
 9 2020-03-02    2020-02-27          4 days       
10 2020-03-03    2020-02-27          5 days       
# ... with 1,056 more rows

```
This gets converted into the desired form for the likelihood function:
```
> h_nr_tibble <- make_h_nr_tibble(delay_data)
> h_nr_tibble
# A tibble: 605 x 3
       n     r  h_nr
   <dbl> <dbl> <int>
 1     0    21     1
 2     7    12     1
 3     9    21     1
 4    12    18     1
 5    25    27     1
 6    30    36     1
 7    31    53     1
 8    34    38     1
 9    36    58     1
10    37    64     1
# ... with 595 more rows

```
and the maximum likelihood calculation is then run using:
```
MLE <- nlm(f = negLL_Weibull_counts_tibble,
           p = c(3,15),
           h_nr_tibble = h_nr_tibble)
```

