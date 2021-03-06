% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_generation_functions.R
\name{select_prob}
\alias{select_prob}
\title{Compute selection probablility.}
\usage{
select_prob(p_value, bias_strength, bias_table)
}
\arguments{
\item{p_value}{p_value (one tailed)}

\item{bias_strength}{String indicating the bias strength}

\item{bias_table}{List of pvalue cut-offs and corresponding selection probabilities
list element names must correspond to elements of bias_strength}
}
\value{
Returns probabilty of publication
}
\description{
Function computes selection probability based on p_value and intended bia strength.
}
