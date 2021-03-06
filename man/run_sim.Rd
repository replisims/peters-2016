% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_sim.R
\name{run_sim}
\alias{run_sim}
\title{Run a prespecified simulation subset}
\usage{
run_sim(iteration_range, scenarios, scenario_selection = 1:nrow(scenarios))
}
\arguments{
\item{iteration_range}{Numerical vector of iterations to be run}

\item{scenarios}{dataframe specifying simulation scenarios i.e.
constellations of simulation paramters}

\item{scenario_selection}{Numerical vector indicating which rows of the
scenarios dataframe should be simulated.
Defaults to entire dataframe}
}
\value{
Returns a list of dataframes containing the simulated meta-analyses
}
\description{
Run a prespecified simulation subset
}
