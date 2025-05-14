#' Vectorized preprocessing of text
#'
#' This function preprocesses text data using vectorized operations for better performance.
#'
#' @param text_data A data frame containing text data.
#' @param text_column Name of the column containing text to process.
#' @param remove_stopwords Logical. If TRUE, removes stopwords.
#' @param custom_stopwords Character vector of additional stopwords to remove.
#' @param min_word_length Minimum word length to keep.
#' @param max_word_length Maximum word length to keep.
#' @param chunk_size Number of documents to process in each chunk.
#'
#' @return A data frame with processed text.
#' @export
#'
#' @examples
#' \dontrun{
#' processed_data <- vec_preprocess(article_data, text_column = "abstract")
#' }
vec_preprocess <- function(text_data, text_column = "abstract",
                           remove_stopwords = TRUE,
                           custom_stopwords = NULL,
                           min_word_length = 3,
                           max_word_length = 50,
                           chunk_size = 100) {

  # Check if text column exists
  if (!text_column %in% colnames(text_data)) {
    stop("Text column '", text_column, "' not found in the data")
  }

  # Add ID column if not present
  if (!"doc_id" %in% colnames(text_data)) {
    text_data$doc_id <- seq_len(nrow(text_data))
  }

  # Create a copy of the data
  processed_data <- text_data

  # Ensure text is character
  processed_data[[text_column]] <- as.character(processed_data[[text_column]])

  # Remove missing values
  processed_data <- processed_data[!is.na(processed_data[[text_column]]), ]

  # Check if processed_data is empty after removing NA values
  if (nrow(processed_data) == 0) {
    warning("No valid text data after removing NA values")
    return(processed_data)
  }

  # Load standard English stopwords if needed
  stopword_list <- character(0)
  if (remove_stopwords) {
    # Define a basic set of English stopwords
    stopword_list <- c(
      "a", "an", "and", "are", "as", "at", "be", "but", "by", "for", "from", "had",
      "has", "have", "he", "her", "his", "i", "in", "is", "it", "its", "of", "on",
      "or", "that", "the", "this", "to", "was", "were", "which", "with", "you"
    )

    # Add custom stopwords if provided
    if (!is.null(custom_stopwords)) {
      stopword_list <- c(stopword_list, custom_stopwords)
    }
  }

  # Process in chunks for memory efficiency
  n_chunks <- ceiling(nrow(processed_data) / chunk_size)
  message("Processing text in ", n_chunks, " chunks...")

  # Initialize progress bar
  pb <- utils::txtProgressBar(min = 0, max = n_chunks, style = 3)

  # Initialize list to store term data frames
  all_term_dfs <- vector("list", nrow(processed_data))

  # Process each chunk
  for (chunk_idx in 1:n_chunks) {
    # Update progress bar
    utils::setTxtProgressBar(pb, chunk_idx)

    # Calculate chunk range
    start_idx <- (chunk_idx - 1) * chunk_size + 1
    end_idx <- min(chunk_idx * chunk_size, nrow(processed_data))

    # Extract current chunk
    chunk <- processed_data[start_idx:end_idx, ]

    # Process each document in the chunk
    for (i in 1:nrow(chunk)) {
      doc_idx <- start_idx + i - 1
      doc_id <- chunk$doc_id[i]
      text <- chunk[[text_column]][i]

      # Skip empty text
      if (is.na(text) || text == "") {
        all_term_dfs[[doc_idx]] <- data.frame(
          word = character(0),
          count = numeric(0),
          stringsAsFactors = FALSE
        )
        next
      }

      # Preprocess text
      # Convert to lowercase
      text <- tolower(text)

      # Replace non-alphanumeric characters with spaces
      text <- gsub("[^a-zA-Z0-9]", " ", text)

      # Split by whitespace
      words <- unlist(strsplit(text, "\\s+"))

      # Remove empty strings
      words <- words[words != ""]

      # Apply length filtering
      words <- words[nchar(words) >= min_word_length & nchar(words) <= max_word_length]

      # Remove stopwords if requested
      if (remove_stopwords) {
        words <- words[!words %in% stopword_list]
      }

      # Count word frequencies
      if (length(words) > 0) {
        # Fast word count using table function
        word_counts <- table(words)

        # Convert to data frame
        term_df <- data.frame(
          word = names(word_counts),
          count = as.numeric(word_counts),
          stringsAsFactors = FALSE
        )

        all_term_dfs[[doc_idx]] <- term_df
      } else {
        all_term_dfs[[doc_idx]] <- data.frame(
          word = character(0),
          count = numeric(0),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Close progress bar
  close(pb)

  # Add terms to the processed data
  processed_data$terms <- all_term_dfs

  return(processed_data)
}

#' Calculate document similarity using TF-IDF and cosine similarity
#'
#' This function calculates the similarity between documents using TF-IDF weighting
#' and cosine similarity.
#'
#' @param text_data A data frame containing text data.
#' @param text_column Name of the column containing text to analyze.
#' @param min_term_freq Minimum frequency for a term to be included.
#' @param max_doc_freq Maximum document frequency (as a proportion) for a term to be included.
#'
#' @return A similarity matrix for the documents.
#' @export
#'
#' @examples
#' \dontrun{
#' sim_matrix <- calc_doc_sim(article_data, text_column = "abstract")
#' }
calc_doc_sim <- function(text_data, text_column = "abstract",
                         min_term_freq = 2, max_doc_freq = 0.9) {

  # Check if text column exists
  if (!text_column %in% colnames(text_data)) {
    stop("Text column '", text_column, "' not found in the data")
  }

  # Preprocess text
  preprocessed <- vec_preprocess(text_data,
                                             text_column = text_column,
                                             remove_stopwords = TRUE)

  # Extract terms from preprocessed data
  all_terms <- unique(unlist(lapply(preprocessed$terms, function(df) df$word)))

  # Create document-term matrix
  dtm <- matrix(0, nrow = nrow(preprocessed), ncol = length(all_terms))
  colnames(dtm) <- all_terms
  rownames(dtm) <- preprocessed$doc_id

  # Fill the document-term matrix
  for (i in 1:nrow(preprocessed)) {
    terms_df <- preprocessed$terms[[i]]
    if (nrow(terms_df) > 0) {
      for (j in 1:nrow(terms_df)) {
        term <- terms_df$word[j]
        count <- terms_df$count[j]
        term_idx <- which(colnames(dtm) == term)
        if (length(term_idx) > 0) {
          dtm[i, term_idx] <- count
        }
      }
    }
  }

  # Calculate term frequency (TF)
  # Normalize by document length
  row_sums <- rowSums(dtm)
  tf <- dtm / row_sums

  # Replace NaN with 0 for empty documents
  tf[is.nan(tf)] <- 0

  # Calculate document frequency (DF)
  df <- colSums(dtm > 0)

  # Filter terms by document frequency
  min_df <- min_term_freq
  max_df <- max_doc_freq * nrow(dtm)

  valid_terms <- which(df >= min_df & df <= max_df)

  if (length(valid_terms) == 0) {
    stop("No terms remain after filtering by document frequency")
  }

  # Subset the matrix to include only valid terms
  tf <- tf[, valid_terms, drop = FALSE]
  df <- df[valid_terms]

  # Calculate inverse document frequency (IDF)
  idf <- log(nrow(dtm) / df)

  # Calculate TF-IDF
  tfidf <- tf * matrix(idf, nrow = nrow(tf), ncol = length(idf), byrow = TRUE)

  # Calculate document similarity using cosine similarity
  # First normalize the vectors to unit length
  norm_tfidf <- tfidf / sqrt(rowSums(tfidf^2))

  # Replace NaN with 0 for zero vectors
  norm_tfidf[is.nan(norm_tfidf)] <- 0

  # Calculate similarity matrix
  sim_matrix <- norm_tfidf %*% t(norm_tfidf)

  # Ensure values are in [0, 1] due to floating-point precision
  sim_matrix[sim_matrix > 1] <- 1
  sim_matrix[sim_matrix < 0] <- 0

  return(sim_matrix)
}

#' Cluster documents using K-means
#'
#' This function clusters documents using K-means based on their TF-IDF vectors.
#'
#' @param text_data A data frame containing text data.
#' @param text_column Name of the column containing text to analyze.
#' @param n_clusters Number of clusters to create.
#' @param min_term_freq Minimum frequency for a term to be included.
#' @param max_doc_freq Maximum document frequency (as a proportion) for a term to be included.
#' @param random_seed Seed for random number generation (for reproducibility).
#'
#' @return A data frame with the original data and cluster assignments.
#' @export
#'
#' @examples
#' \dontrun{
#' clustered_data <- cluster_docs(article_data, text_column = "abstract", n_clusters = 5)
#' }
cluster_docs <- function(text_data, text_column = "abstract",
                         n_clusters = 5, min_term_freq = 2, max_doc_freq = 0.9,
                         random_seed = 42) {

  # Check if text column exists
  if (!text_column %in% colnames(text_data)) {
    stop("Text column '", text_column, "' not found in the data")
  }

  # Preprocess text
  preprocessed <- vec_preprocess(text_data,
                                             text_column = text_column,
                                             remove_stopwords = TRUE)

  # Extract terms from preprocessed data
  all_terms <- unique(unlist(lapply(preprocessed$terms, function(df) df$word)))

  # Create document-term matrix
  dtm <- matrix(0, nrow = nrow(preprocessed), ncol = length(all_terms))
  colnames(dtm) <- all_terms
  rownames(dtm) <- preprocessed$doc_id

  # Fill the document-term matrix
  for (i in 1:nrow(preprocessed)) {
    terms_df <- preprocessed$terms[[i]]
    if (nrow(terms_df) > 0) {
      for (j in 1:nrow(terms_df)) {
        term <- terms_df$word[j]
        count <- terms_df$count[j]
        term_idx <- which(colnames(dtm) == term)
        if (length(term_idx) > 0) {
          dtm[i, term_idx] <- count
        }
      }
    }
  }

  # Calculate TF-IDF
  # Term frequency (TF)
  row_sums <- rowSums(dtm)
  tf <- dtm / row_sums
  tf[is.nan(tf)] <- 0

  # Document frequency (DF)
  df <- colSums(dtm > 0)

  # Filter terms by document frequency
  min_df <- min_term_freq
  max_df <- max_doc_freq * nrow(dtm)

  valid_terms <- which(df >= min_df & df <= max_df)

  if (length(valid_terms) == 0) {
    stop("No terms remain after filtering by document frequency")
  }

  # Subset the matrix to include only valid terms
  tf <- tf[, valid_terms, drop = FALSE]
  df <- df[valid_terms]

  # Inverse document frequency (IDF)
  idf <- log(nrow(dtm) / df)

  # TF-IDF
  tfidf <- tf * matrix(idf, nrow = nrow(tf), ncol = length(idf), byrow = TRUE)

  # Set seed for reproducibility
  set.seed(random_seed)

  # Apply K-means clustering
  kmeans_result <- stats::kmeans(tfidf, centers = n_clusters, nstart = 25)

  # Add cluster assignments to the original data
  clustered_data <- preprocessed
  clustered_data$cluster <- kmeans_result$cluster

  # Identify top terms for each cluster
  cluster_terms <- list()
  for (i in 1:n_clusters) {
    # Get documents in this cluster
    cluster_docs <- which(kmeans_result$cluster == i)

    # Skip empty clusters
    if (length(cluster_docs) == 0) {
      cluster_terms[[i]] <- data.frame(
        term = character(0),
        score = numeric(0),
        stringsAsFactors = FALSE
      )
      next
    }

    # Calculate mean TF-IDF for each term in the cluster
    cluster_tfidf <- colMeans(tfidf[cluster_docs, , drop = FALSE])

    # Sort terms by score
    sorted_terms <- sort(cluster_tfidf, decreasing = TRUE)

    # Select top terms
    top_n_terms <- min(20, length(sorted_terms))
    top_terms <- sorted_terms[1:top_n_terms]

    # Create data frame of top terms
    cluster_terms[[i]] <- data.frame(
      term = names(top_terms),
      score = top_terms,
      stringsAsFactors = FALSE
    )
  }

  # Add cluster summaries
  cluster_summaries <- lapply(1:n_clusters, function(i) {
    cluster_size <- sum(kmeans_result$cluster == i)
    top_terms <- if (nrow(cluster_terms[[i]]) > 0)
      paste(cluster_terms[[i]]$term[1:min(5, nrow(cluster_terms[[i]]))], collapse = ", ")
    else
      "NA"

    return(paste0("Cluster ", i, " (", cluster_size, " docs): ", top_terms))
  })

  attr(clustered_data, "cluster_terms") <- cluster_terms
  attr(clustered_data, "cluster_summaries") <- cluster_summaries
  attr(clustered_data, "kmeans_result") <- kmeans_result

  return(clustered_data)
}

#' Find similar documents for a given document
#'
#' This function finds documents similar to a given document based on TF-IDF and
#' cosine similarity.
#'
#' @param text_data A data frame containing text data.
#' @param doc_id ID of the document to find similar documents for.
#' @param text_column Name of the column containing text to analyze.
#' @param n_similar Number of similar documents to return.
#'
#' @return A data frame with similar documents and their similarity scores.
#' @export
#'
#' @examples
#' \dontrun{
#' similar_docs <- find_similar_docs(article_data, doc_id = 1,
#'                                      text_column = "abstract", n_similar = 5)
#' }
find_similar_docs <- function(text_data, doc_id, text_column = "abstract",
                              n_similar = 5) {

  # Check if doc_id exists
  if (!"doc_id" %in% colnames(text_data)) {
    text_data$doc_id <- seq_len(nrow(text_data))
  }

  if (!doc_id %in% text_data$doc_id) {
    stop("Document ID '", doc_id, "' not found in the data")
  }

  # Calculate document similarity matrix
  sim_matrix <- calc_doc_sim(text_data, text_column)

  # Get row index for the target document
  doc_idx <- which(rownames(sim_matrix) == as.character(doc_id))

  if (length(doc_idx) == 0) {
    stop("Document ID '", doc_id, "' not found in similarity matrix")
  }

  # Get similarities for the target document
  sim_scores <- sim_matrix[doc_idx, ]

  # Sort similarities (excluding the document itself)
  sim_scores[doc_idx] <- 0  # Set self-similarity to 0
  sorted_indices <- order(sim_scores, decreasing = TRUE)

  # Get top N similar documents
  top_n <- min(n_similar, length(sorted_indices))
  top_indices <- sorted_indices[1:top_n]
  top_doc_ids <- as.numeric(rownames(sim_matrix)[top_indices])
  top_scores <- sim_scores[top_indices]

  # Create result data frame
  result <- data.frame(
    doc_id = top_doc_ids,
    similarity_score = top_scores,
    stringsAsFactors = FALSE
  )

  # Add document metadata
  if (all(c("title", "publication_year") %in% colnames(text_data))) {
    result$title <- sapply(result$doc_id, function(id) {
      text_data$title[text_data$doc_id == id]
    })

    result$publication_year <- sapply(result$doc_id, function(id) {
      text_data$publication_year[text_data$doc_id == id]
    })
  }

  return(result)
}
