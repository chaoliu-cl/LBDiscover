% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_preprocessing.R
\name{validate_umls_key}
\alias{validate_umls_key}
\title{Validate a UMLS API key}
\usage{
validate_umls_key(api_key, validator_api_key = NULL)
}
\arguments{
\item{api_key}{UMLS API key to validate}

\item{validator_api_key}{Your application's UMLS API key (for third-party validation)}
}
\value{
Logical indicating if the API key is valid
}
\description{
This function validates a UMLS API key using the validation endpoint.
}
\examples{
\dontrun{
is_valid <- validate_umls_key("user_api_key")
}
}
