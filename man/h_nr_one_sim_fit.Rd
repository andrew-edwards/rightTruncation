% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/likelihood.R
\name{h_nr_one_sim_fit}
\alias{h_nr_one_sim_fit}
\title{Wrapper function for simulating one h_nr matrix and fitting with likelihood
using \code{nlm()}}
\usage{
h_nr_one_sim_fit(init = c(3, 15), ...)
}
\arguments{
\item{init}{initial conditions for optimisation}

\item{...}{inputs for \code{h_nr_simulate()}}
}
\value{
MLE_res output from nlm, a list containing components:
\itemize{
\item \code{minimum}: minimum of negative log-likelihod
\item \code{estimate}: vector of MLEs of \code{k} and then \code{lambda}
\item \code{gradient}: vector of the gradient at the minimum (should be close to \code{c(0,0)})
\item \code{code}: integer indicating why the optimization process terminated, 1 is
relative gradient is close to zero (see \code{?nlm}).
\item \code{iterations}: number of iterations performed
}
}
\description{
Wrapper function for simulating one h_nr matrix and fitting with likelihood
using \code{nlm()}
}
\author{
Andrew Edwards
}
