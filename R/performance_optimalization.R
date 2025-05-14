#' Create a sparse co-occurrence matrix
#'
#' This function creates a sparse co-occurrence matrix from entity data,
#' which is more memory-efficient for large datasets.
#'
#' @param entity_data A data frame with document IDs and entities.
#' @param doc_id_col Name of the column containing document IDs.
#' @param entity_col Name of the column containing entity names.
#' @param count_col Name of the column containing entity counts (optional).
#' @param type_col Name of the column containing entity types (optional).
#' @param normalize Logical. If TRUE, normalizes the co-occurrence matrix.
#'
#' @return A sparse matrix of entity co-occurrences.
#' @export
#'
#' @examples
#' \dontrun{
#' co_matrix <- create_sparse_comat(entities,
#'                                               doc_id_col = "doc_id",
#'                                               entity_col = "entity")
#' }
create_sparse_comat <- function(entity_data,
                                doc_id_col = "doc_id",
                                entity_col = "entity",
                                count_col = NULL,
                                type_col = NULL,
                                normalize = TRUE) {

  # Check if required columns exist
  required_cols <- c(doc_id_col, entity_col)
  if (!all(required_cols %in% colnames(entity_data))) {
    stop("Required columns not found in the data: ",
         paste(required_cols[!required_cols %in% colnames(entity_data)], collapse = ", "))
  }

  # Extract unique documents and entities
  docs <- unique(entity_data[[doc_id_col]])
  entities <- unique(entity_data[[entity_col]])

  # Ensure docs and entities are character vectors for safer indexing
  docs <- as.character(docs)
  entities <- as.character(entities)

  # Create sparse matrix for efficiency
  require_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(pkg, " package is required. Install it with: install.packages('", pkg, "')")
    }
  }

  require_package("Matrix")

  # Initialize triplets for sparse matrix construction
  i <- integer(0)  # Row indices
  j <- integer(0)  # Column indices
  x <- numeric(0)  # Values

  # Document to index mapping
  doc_to_idx <- setNames(seq_along(docs), docs)

  # Entity to index mapping
  entity_to_idx <- setNames(seq_along(entities), entities)

  # Fill the entity-document matrix using triplets
  message("Building entity-document matrix...")
  pb <- utils::txtProgressBar(min = 0, max = nrow(entity_data), style = 3)

  chunk_size <- 1000  # Process in chunks for better progress reporting
  for (chunk_start in seq(1, nrow(entity_data), by = chunk_size)) {
    chunk_end <- min(chunk_start + chunk_size - 1, nrow(entity_data))
    chunk <- entity_data[chunk_start:chunk_end, ]

    for (k in 1:nrow(chunk)) {
      doc <- as.character(chunk[[doc_id_col]][k])
      entity <- as.character(chunk[[entity_col]][k])

      # Skip if the document or entity doesn't exist in our mappings
      if (!doc %in% names(doc_to_idx) || !entity %in% names(entity_to_idx)) {
        warning("Document ID '", doc, "' or entity '", entity, "' not found in mapping. Skipping.")
        next
      }

      # Get the value to fill
      if (!is.null(count_col) && count_col %in% colnames(chunk)) {
        value <- chunk[[count_col]][k]
      } else {
        value <- 1  # Binary presence
      }

      # Add triplet
      i <- c(i, doc_to_idx[doc])
      j <- c(j, entity_to_idx[entity])
      x <- c(x, value)

      utils::setTxtProgressBar(pb, chunk_start + k - 1)
    }
  }

  close(pb)

  # Create sparse entity-document matrix
  message("Creating sparse matrix...")
  entity_doc_matrix <- Matrix::sparseMatrix(
    i = i,
    j = j,
    x = x,
    dims = c(length(docs), length(entities)),
    dimnames = list(docs, entities)
  )

  # Calculate co-occurrence matrix: t(A) %*% A
  message("Calculating co-occurrence matrix...")
  co_matrix <- Matrix::t(entity_doc_matrix) %*% entity_doc_matrix

  # Set diagonal to zero (entities don't co-occur with themselves)
  diag(co_matrix) <- 0

  # Normalize if requested
  if (normalize) {
    message("Normalizing co-occurrence matrix...")
    # Get the frequency of each entity
    entity_freq <- Matrix::diag(Matrix::t(entity_doc_matrix) %*% entity_doc_matrix)

    # Create normalization matrix
    norm_matrix <- sqrt(outer(entity_freq, entity_freq))

    # Avoid division by zero
    norm_matrix[norm_matrix == 0] <- 1

    # Normalize the co-occurrence matrix (similar to cosine similarity)
    co_matrix <- co_matrix / norm_matrix
  }

  # If type column is provided, add entity types as an attribute
  if (!is.null(type_col) && type_col %in% colnames(entity_data)) {
    # Create a mapping of entity to type
    entity_types <- tapply(entity_data[[type_col]], entity_data[[entity_col]], function(x) x[1])

    # Add as an attribute to the matrix
    attr(co_matrix, "entity_types") <- entity_types
  }

  return(co_matrix)
}

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

#' Apply parallel processing for document analysis
#'
#' This function uses parallel processing to analyze documents faster.
#'
#' @param text_data A data frame containing text data.
#' @param analysis_function Function to apply to each document.
#' @param text_column Name of the column containing text to analyze.
#' @param ... Additional arguments passed to the analysis function.
#' @param n_cores Number of cores to use for parallel processing. If NULL, uses all available cores minus 1.
#'
#' @return A data frame with analysis results.
#' @export
#'
#' @examples
#' \dontrun{
#' # Define a simple analysis function
#' count_words <- function(text) {
#'   words <- unlist(strsplit(tolower(text), "\\s+"))
#'   return(length(words))
#' }
#'
#' # Apply parallel processing
#' results <- parallel_analysis(article_data, count_words, text_column = "abstract")
#' }
parallel_analysis <- function(text_data, analysis_function, text_column = "abstract",
                              ..., n_cores = NULL) {

  # Check if parallel package is available
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("The parallel package is required. Install it with: install.packages('parallel')")
  }

  # Check if text column exists
  if (!text_column %in% colnames(text_data)) {
    stop("Text column '", text_column, "' not found in the data")
  }

  # Determine number of cores to use
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  # Create a cluster
  message("Starting parallel processing with ", n_cores, " cores...")
  cl <- parallel::makeCluster(n_cores)

  # Export necessary variables to the cluster
  parallel::clusterExport(cl, varlist = c("analysis_function"), envir = environment())

  # Prepare the texts to analyze
  texts <- text_data[[text_column]]

  # Run analysis in parallel
  results <- parallel::parLapply(cl, texts, function(text, ...) {
    # Skip NA values
    if (is.na(text) || text == "") {
      return(NA)
    }

    # Apply the analysis function
    result <- analysis_function(text, ...)
    return(result)
  }, ...)

  # Stop the cluster
  parallel::stopCluster(cl)

  # Add results to the original data frame
  text_data$analysis_result <- results

  return(text_data)
}

#' Optimize ABC model calculations for large matrices
#'
#' This function implements an optimized version of the ABC model calculation
#' that's more efficient for large co-occurrence matrices.
#'
#' @param co_matrix A co-occurrence matrix produced by create_cooccurrence_matrix().
#' @param a_term Character string, the source term (A).
#' @param c_term Character string, the target term (C). If NULL, all potential C terms will be evaluated.
#' @param min_score Minimum score threshold for results.
#' @param n_results Maximum number of results to return.
#' @param chunk_size Number of B terms to process in each chunk.
#'
#' @return A data frame with ranked discovery results.
#' @export
#'
#' @examples
#' \dontrun{
#' abc_results <- abc_model_opt(co_matrix, a_term = "migraine")
#' }
abc_model_opt <- function(co_matrix, a_term, c_term = NULL,
                          min_score = 0.1, n_results = 100,
                          chunk_size = 500) {

  # Check if the matrix has entity types
  has_entity_types <- !is.null(attr(co_matrix, "entity_types"))

  # Check if A term exists in the matrix
  if (!a_term %in% rownames(co_matrix)) {
    stop("A-term '", a_term, "' not found in the co-occurrence matrix")
  }

  # Check if C term exists (if provided)
  if (!is.null(c_term) && !c_term %in% rownames(co_matrix)) {
    stop("C-term '", c_term, "' not found in the co-occurrence matrix")
  }

  # Extract A-B associations
  a_associations <- co_matrix[a_term, ]

  # Filter B terms by removing terms with low association to A
  b_terms <- names(a_associations[a_associations > min_score])

  # Remove A term from B terms if present
  b_terms <- b_terms[b_terms != a_term]

  # If no B terms found, return empty result
  if (length(b_terms) == 0) {
    message("No B terms found with association score > ", min_score)
    return(data.frame(
      a_term = character(),
      b_term = character(),
      c_term = character(),
      a_b_score = numeric(),
      b_c_score = numeric(),
      abc_score = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # Initialize results
  results <- data.frame(
    a_term = character(),
    b_term = character(),
    c_term = character(),
    a_b_score = numeric(),
    b_c_score = numeric(),
    abc_score = numeric(),
    stringsAsFactors = FALSE
  )

  # If a specific C term is provided
  if (!is.null(c_term)) {
    message("Processing connections with specified C term: ", c_term)

    # Process in chunks for memory efficiency
    n_chunks <- ceiling(length(b_terms) / chunk_size)

    for (chunk_idx in 1:n_chunks) {
      # Calculate chunk range
      start_idx <- (chunk_idx - 1) * chunk_size + 1
      end_idx <- min(chunk_idx * chunk_size, length(b_terms))

      # Extract B terms for this chunk
      chunk_b_terms <- b_terms[start_idx:end_idx]

      # Get B-C scores all at once (vectorized)
      b_c_scores <- co_matrix[chunk_b_terms, c_term]

      # Filter terms with sufficient B-C score
      valid_indices <- which(b_c_scores > min_score)

      if (length(valid_indices) > 0) {
        valid_b_terms <- chunk_b_terms[valid_indices]
        valid_b_c_scores <- b_c_scores[valid_indices]

        # Get A-B scores for valid B terms
        valid_a_b_scores <- a_associations[valid_b_terms]

        # Calculate ABC scores
        abc_scores <- valid_a_b_scores * valid_b_c_scores

        # Create chunk results
        chunk_results <- data.frame(
          a_term = rep(a_term, length(valid_b_terms)),
          b_term = valid_b_terms,
          c_term = rep(c_term, length(valid_b_terms)),
          a_b_score = valid_a_b_scores,
          b_c_score = valid_b_c_scores,
          abc_score = abc_scores,
          stringsAsFactors = FALSE
        )

        # Combine with overall results
        results <- rbind(results, chunk_results)
      }
    }
  } else {
    # Explore all potential C terms
    message("Exploring all potential C terms")

    # Get all terms except A
    all_terms <- rownames(co_matrix)
    potential_c_terms <- setdiff(all_terms, a_term)

    # Process B terms in chunks
    n_chunks <- ceiling(length(b_terms) / chunk_size)
    message("Processing ", length(b_terms), " B terms in ", n_chunks, " chunks...")
    pb <- utils::txtProgressBar(min = 0, max = n_chunks, style = 3)

    for (chunk_idx in 1:n_chunks) {
      utils::setTxtProgressBar(pb, chunk_idx)

      # Calculate chunk range
      start_idx <- (chunk_idx - 1) * chunk_size + 1
      end_idx <- min(chunk_idx * chunk_size, length(b_terms))

      # Extract B terms for this chunk
      chunk_b_terms <- b_terms[start_idx:end_idx]

      # Get A-B scores for this chunk
      chunk_a_b_scores <- a_associations[chunk_b_terms]

      # For each B term in the chunk
      for (i in seq_along(chunk_b_terms)) {
        b_term <- chunk_b_terms[i]
        a_b_score <- chunk_a_b_scores[i]

        # Get all B-C associations at once
        b_associations <- co_matrix[b_term, ]

        # Filter C terms with sufficient score and not equal to A or B
        potential_c_for_b <- names(b_associations[b_associations > min_score])
        potential_c_for_b <- setdiff(potential_c_for_b, c(a_term, b_term))

        if (length(potential_c_for_b) > 0) {
          # Get B-C scores
          b_c_scores <- b_associations[potential_c_for_b]

          # Calculate ABC scores
          abc_scores <- a_b_score * b_c_scores

          # Create results for this B term
          b_results <- data.frame(
            a_term = rep(a_term, length(potential_c_for_b)),
            b_term = rep(b_term, length(potential_c_for_b)),
            c_term = potential_c_for_b,
            a_b_score = rep(a_b_score, length(potential_c_for_b)),
            b_c_score = b_c_scores,
            abc_score = abc_scores,
            stringsAsFactors = FALSE
          )

          # Combine with overall results
          results <- rbind(results, b_results)
        }
      }
    }

    close(pb)
  }

  # If no results found, return empty data frame
  if (nrow(results) == 0) {
    message("No ABC connections found")
    return(results)
  }

  # Sort by ABC score and limit to n_results
  results <- results[order(-results$abc_score), ]
  if (nrow(results) > n_results) {
    results <- results[1:n_results, ]
  }

  # Add entity type information if available
  if (has_entity_types) {
    entity_types <- attr(co_matrix, "entity_types")

    results$a_type <- entity_types[results$a_term]
    results$b_type <- entity_types[results$b_term]
    results$c_type <- entity_types[results$c_term]
  }

  return(results)
}
