% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_generation_functions.R
\name{simulate_unbiased_study_set}
\alias{simulate_unbiased_study_set}
\title{Simulate full unbiased study set.}
\usage{
simulate_unbiased_study_set(
  job_id,
  scenario_id,
  ma_size,
  p_contr,
  odds_ratio,
  bias_type,
  bias_strength = NULL,
  bias_percentage = NULL,
  n_cg_distr,
  bias_table,
  tau_squared = 0
)
}
\arguments{
\item{ma_size}{passed on to \code{obtain_true_ma_size()}}

\item{p_contr}{passed on to \code{simulate_study()}}

\item{odds_ratio}{passed on to \code{simulate_study()}}

\item{bias_type}{paassed on to \code{simulate_study()} and \code{obtain_true_ma_size()}}

\item{bias_strength}{passed on to \code{simulate_study()}}

\item{bias_percentage}{passed on to \code{obtain_true_ma_size()}}

\item{n_cg_distr}{passed on to \code{simulate_study()}}

\item{tau_squared}{passed on to \code{simulate_study()}}
}
\value{
Returns a data frame of all studies pertaining to a given
  meta-analysis before publication bias
}
\description{
Repeatedly calls \code{simulate_study()} until the intended meta-analysis size equals the
number of studies with a positive selection indicator (== 1)
}
