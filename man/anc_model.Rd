% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alternative_models.R
\name{anc_model}
\alias{anc_model}
\title{ANC model for literature-based discovery with biomedical term filtering}
\usage{
anc_model(
  co_matrix,
  a_term,
  n_b_terms = 3,
  c_type = NULL,
  min_score = 0.1,
  n_results = 100,
  enforce_biomedical_terms = TRUE,
  b_term_types = c("protein", "gene", "chemical", "pathway", "drug", "disease",
    "biological_process"),
  validation_function = is_valid_biomedical_entity
)
}
\arguments{
\item{co_matrix}{A co-occurrence matrix produced by create_cooccurrence_matrix().}

\item{a_term}{Character string, the source term (A).}

\item{n_b_terms}{Number of intermediate B terms to consider.}

\item{c_type}{Character string, the entity type for C terms. If NULL, all types are considered.}

\item{min_score}{Minimum score threshold for results.}

\item{n_results}{Maximum number of results to return.}

\item{enforce_biomedical_terms}{Logical. If TRUE, enforces strict biomedical term filtering.}

\item{b_term_types}{Character vector of entity types allowed for B terms.}

\item{validation_function}{Function to validate biomedical terms.}
}
\value{
A data frame with ranked discovery results.
}
\description{
This function implements an improved ANC model that ensures only biomedical
terms are used as intermediaries.
}
