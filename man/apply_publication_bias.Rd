% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_generation_functions.R
\name{apply_publication_bias}
\alias{apply_publication_bias}
\title{Apply publication bias.}
\usage{
apply_publication_bias(ma_data, ma_size, bias_type)
}
\arguments{
\item{ma_data}{Dataset with simulated studies before publication bias.}

\item{bias_type}{Bias type can either be "p" or "es".}
}
\value{
Returns biased set of simulated study.
}
\description{
Function turns unbiased set of studies into biased one (if applicable).
}
