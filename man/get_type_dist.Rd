% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abc_model.R
\name{get_type_dist}
\alias{get_type_dist}
\title{Get entity type distribution from co-occurrence matrix}
\usage{
get_type_dist(co_matrix)
}
\arguments{
\item{co_matrix}{A co-occurrence matrix produced by create_typed_comat().}
}
\value{
A data frame with entity type counts and percentages.
}
\description{
Get entity type distribution from co-occurrence matrix
}
\examples{
\dontrun{
type_dist <- get_type_dist(co_matrix)
}
}
