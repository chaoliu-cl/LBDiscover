% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_preprocessing.R
\name{create_term_document_matrix}
\alias{create_term_document_matrix}
\title{Create a term-document matrix from preprocessed text}
\usage{
create_term_document_matrix(preprocessed_data, min_df = 2, max_df = 0.9)
}
\arguments{
\item{preprocessed_data}{A data frame with preprocessed text data.}

\item{min_df}{Minimum document frequency for a term to be included.}

\item{max_df}{Maximum document frequency (as a proportion) for a term to be included.}
}
\value{
A term-document matrix.
}
\description{
This function creates a term-document matrix from preprocessed text data.
It's a simplified version of create_tdm() for direct use in models.
}
\examples{
\dontrun{
preprocessed <- preprocess_text(article_data, text_column = "abstract")
tdm <- create_term_document_matrix(preprocessed)
}
}
