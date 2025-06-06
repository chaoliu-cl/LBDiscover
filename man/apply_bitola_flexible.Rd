% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comprehensive_summary.R
\name{apply_bitola_flexible}
\alias{apply_bitola_flexible}
\title{Apply a flexible BITOLA-style discovery model without strict type constraints}
\usage{
apply_bitola_flexible(co_matrix, a_term, min_score = 0.1, n_results = 100)
}
\arguments{
\item{co_matrix}{A co-occurrence matrix with entity types as an attribute.}

\item{a_term}{Character string, the source term (A).}

\item{min_score}{Minimum score threshold for results.}

\item{n_results}{Maximum number of results to return.}
}
\value{
A data frame with ranked discovery results.
}
\description{
This function implements a modified BITOLA-style discovery model that preserves
entity type information but doesn't enforce strict type constraints.
}
\keyword{internal}
