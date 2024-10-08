% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_children.R
\name{pivot_children}
\alias{pivot_children}
\title{Pivot ELAN annotations from child tiers to a wide format}
\usage{
pivot_children(data, long = FALSE)
}
\arguments{
\item{data}{A tibble of annotations}

\item{long}{Whether the child tiers are pivoted back to long format (default = `FALSE`)}
}
\value{
A tibble with dependent (child) annotations in wide format
}
\description{
Pivot ELAN annotations from child tiers to a wide format
}
