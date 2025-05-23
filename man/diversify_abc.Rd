% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abc_model.R
\name{diversify_abc}
\alias{diversify_abc}
\title{Enforce diversity in ABC model results}
\usage{
diversify_abc(
  abc_results,
  diversity_method = c("both", "b_term_groups", "unique_c_paths"),
  max_per_group = 3,
  min_score = 0.1
)
}
\arguments{
\item{abc_results}{A data frame containing ABC results.}

\item{diversity_method}{Method for enforcing diversity: "b_term_groups", "unique_c_paths", or "both".}

\item{max_per_group}{Maximum number of results to keep per B term or C term.}

\item{min_score}{Minimum score threshold for including connections.}
}
\value{
A data frame with diverse ABC results.
}
\description{
This function applies diversity enforcement to ABC model results by:
\enumerate{
\item Removing duplicate paths to the same C term
\item Ensuring B term diversity by selecting top results from each B term group
\item Preventing A and C terms from appearing as B terms
}
}
\examples{
\dontrun{
diverse_results <- diversify_abc(abc_results)
}
}
