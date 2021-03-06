% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures.R
\name{plot_projections}
\alias{plot_projections}
\title{projections plot with median and various quantiles}
\usage{
plot_projections(obs, pred)
}
\arguments{
\item{obs}{data frame with columns dates and deaths}

\item{pred}{data frame with columns dates, val, .lower and .upper.
This is an output of ggdist::median_qi function.}
}
\value{
ggplot object
}
\description{
Projections plot
}
\author{
Sangeeta Bhatia
}
