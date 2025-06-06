% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abc_model.R
\name{diversify_c_paths}
\alias{diversify_c_paths}
\title{Enforce diversity for C term paths}
\usage{
diversify_c_paths(results, max_per_c = 3)
}
\arguments{
\item{results}{Data frame with ABC model results}

\item{max_per_c}{Maximum number of paths to keep per C term}
}
\value{
Data frame with C term path diversity enforced
}
\description{
Enforce diversity for C term paths
}
\keyword{internal}
