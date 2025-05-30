% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_preprocessing.R
\name{preprocess_text}
\alias{preprocess_text}
\title{Preprocess article text}
\usage{
preprocess_text(
  text_data,
  text_column = "abstract",
  remove_stopwords = TRUE,
  custom_stopwords = NULL,
  stem_words = FALSE,
  min_word_length = 3,
  max_word_length = 50
)
}
\arguments{
\item{text_data}{A data frame containing article text data (title, abstract, etc.).}

\item{text_column}{Name of the column containing text to process.}

\item{remove_stopwords}{Logical. If TRUE, removes stopwords.}

\item{custom_stopwords}{Character vector of additional stopwords to remove.}

\item{stem_words}{Logical. If TRUE, applies stemming to words.}

\item{min_word_length}{Minimum word length to keep.}

\item{max_word_length}{Maximum word length to keep.}
}
\value{
A data frame with processed text and extracted terms.
}
\description{
This function preprocesses article text for further analysis.
}
\examples{
\dontrun{
processed_data <- preprocess_text(article_data, text_column = "abstract")
}
}
