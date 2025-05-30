% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{merge_entities}
\alias{merge_entities}
\title{Combine and deduplicate entity datasets}
\usage{
merge_entities(
  custom_entities,
  standard_entities,
  primary_term,
  primary_type = "disease",
  verbose = TRUE
)
}
\arguments{
\item{custom_entities}{Data frame of custom entities (can be NULL)}

\item{standard_entities}{Data frame of standard entities (can be NULL)}

\item{primary_term}{The primary term of interest}

\item{primary_type}{The entity type of the primary term (default: "disease")}

\item{verbose}{Logical; if TRUE, print status messages (default: TRUE)}
}
\value{
A data frame of combined entities
}
\description{
This function combines custom and standard entity datasets, handling the case
where one or both might be empty, and removes duplicates.
}
