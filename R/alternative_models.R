#' ANC model for literature-based discovery with biomedical term filtering
#'
#' This function implements an improved ANC model that ensures only biomedical
#' terms are used as intermediaries.
#'
#' @param co_matrix A co-occurrence matrix produced by create_cooccurrence_matrix().
#' @param a_term Character string, the source term (A).
#' @param n_b_terms Number of intermediate B terms to consider.
#' @param c_type Character string, the entity type for C terms. If NULL, all types are considered.
#' @param min_score Minimum score threshold for results.
#' @param n_results Maximum number of results to return.
#' @param enforce_biomedical_terms Logical. If TRUE, enforces strict biomedical term filtering.
#' @param b_term_types Character vector of entity types allowed for B terms.
#' @param validation_function Function to validate biomedical terms.
#'
#' @return A data frame with ranked discovery results.
#' @export
anc_model <- function(co_matrix, a_term, n_b_terms = 3,
                      c_type = NULL, min_score = 0.1, n_results = 100,
                      enforce_biomedical_terms = TRUE,
                      b_term_types = c("protein", "gene", "chemical", "pathway",
                                       "drug", "disease", "biological_process"),
                      validation_function = is_valid_biomedical_entity) {

  # Check if the matrix has entity types
  has_entity_types <- !is.null(attr(co_matrix, "entity_types"))

  # Extract entity types if available
  entity_types <- if (has_entity_types) attr(co_matrix, "entity_types") else NULL

  # Check if A term exists in the matrix
  if (!a_term %in% rownames(co_matrix)) {
    stop("A-term '", a_term, "' not found in the co-occurrence matrix")
  }

  # Extract A-B associations
  a_associations <- co_matrix[a_term, ]

  # Filter B terms by removing terms with low association to A
  b_terms <- names(a_associations[a_associations > min_score])

  # Remove A term from B terms if present
  b_terms <- b_terms[b_terms != a_term]

  # Apply strict biomedical term filtering if requested
  if (enforce_biomedical_terms) {
    # Remove blacklisted terms (using static_data)
    b_terms <- b_terms[!tolower(b_terms) %in% static_data$blacklisted_terms]

    # Filter B terms by entity type if available and types specified
    if (has_entity_types && !is.null(b_term_types)) {
      b_term_type_filter <- function(term) {
        if (term %in% names(entity_types)) {
          return(entity_types[term] %in% b_term_types)
        }
        return(FALSE)
      }

      b_terms <- b_terms[sapply(b_terms, b_term_type_filter)]
    }

    # Additional biomedical entity validation using provided function
    message("Validating biomedical relevance of B terms...")
    valid_b_terms <- character(0)

    for (term in b_terms) {
      term_type <- if (has_entity_types && term %in% names(entity_types)) entity_types[term] else NULL

      # Apply validation function
      if (validation_function(term, term_type)) {
        valid_b_terms <- c(valid_b_terms, term)
      }
    }

    # Update b_terms with validated terms
    b_terms <- valid_b_terms

    message("Retained ", length(b_terms), " biomedically relevant B terms after filtering")
  }

  # If no B terms found, return empty result
  if (length(b_terms) == 0) {
    message("No suitable B terms found with association score > ", min_score)
    return(data.frame(
      a_term = character(),
      b_terms = character(),
      c_term = character(),
      a_b_scores = character(),
      b_c_scores = character(),
      anc_score = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # Sort B terms by association strength and take top n_b_terms
  sorted_b_terms <- b_terms[order(-a_associations[b_terms])]
  if (length(sorted_b_terms) > n_b_terms) {
    sorted_b_terms <- sorted_b_terms[1:n_b_terms]
  }

  # Get all terms except A and B terms
  all_terms <- rownames(co_matrix)
  potential_c_terms <- setdiff(all_terms, c(a_term, sorted_b_terms))

  # Filter by c_type if provided and entity types are available
  if (!is.null(c_type) && has_entity_types) {
    c_type_terms <- names(entity_types[entity_types == c_type])
    potential_c_terms <- intersect(potential_c_terms, c_type_terms)
  }

  # Apply the same biomedical term filtering to C terms
  if (enforce_biomedical_terms) {
    # Remove blacklisted terms from potential C terms (using static_data)
    potential_c_terms <- potential_c_terms[!tolower(potential_c_terms) %in% static_data$blacklisted_terms]

    # Additional biomedical entity validation for C terms
    message("Validating biomedical relevance of C terms...")
    valid_c_terms <- character(0)

    for (term in potential_c_terms) {
      term_type <- if (has_entity_types && term %in% names(entity_types)) entity_types[term] else NULL

      # Apply validation function
      if (validation_function(term, term_type)) {
        valid_c_terms <- c(valid_c_terms, term)
      }
    }

    # Update potential_c_terms with validated terms
    potential_c_terms <- valid_c_terms

    message("Retained ", length(potential_c_terms), " biomedically relevant C terms after filtering")
  }

  # Initialize results
  results <- data.frame(
    a_term = character(),
    b_terms = character(),
    c_term = character(),
    a_b_scores = character(),
    b_c_scores = character(),
    anc_score = numeric(),
    stringsAsFactors = FALSE
  )

  # For each potential C term
  message("Analyzing ", length(potential_c_terms), " potential C terms...")
  pb <- utils::txtProgressBar(min = 0, max = length(potential_c_terms), style = 3)

  for (c_idx in seq_along(potential_c_terms)) {
    utils::setTxtProgressBar(pb, c_idx)

    c_term <- potential_c_terms[c_idx]

    # Check B-C associations for all B terms
    b_c_scores <- co_matrix[sorted_b_terms, c_term]

    # Filter for potential C terms with sufficient association
    valid_b_indices <- which(b_c_scores > min_score)

    if (length(valid_b_indices) >= 2) {  # Require at least 2 B terms
      valid_b_terms <- sorted_b_terms[valid_b_indices]
      valid_b_c_scores <- b_c_scores[valid_b_indices]
      valid_a_b_scores <- a_associations[valid_b_terms]

      # Calculate AnC score
      # Geometric mean of A-B scores and B-C scores
      anc_score <- sqrt(mean(valid_a_b_scores) * mean(valid_b_c_scores)) * length(valid_b_indices) / n_b_terms

      # Create result entry
      results <- rbind(results, data.frame(
        a_term = a_term,
        b_terms = paste(valid_b_terms, collapse = ", "),
        c_term = c_term,
        a_b_scores = paste(round(valid_a_b_scores, 3), collapse = ", "),
        b_c_scores = paste(round(valid_b_c_scores, 3), collapse = ", "),
        anc_score = anc_score,
        stringsAsFactors = FALSE
      ))
    }
  }

  close(pb)

  # If no results found, return empty data frame
  if (nrow(results) == 0) {
    message("No AnC connections found")
    return(results)
  }

  # Sort by AnC score and limit to n_results
  results <- results[order(-results$anc_score), ]
  if (nrow(results) > n_results) {
    results <- results[1:n_results, ]
  }

  # Add entity type information if available
  if (has_entity_types) {
    results$a_type <- entity_types[results$a_term]
    results$c_type <- entity_types[results$c_term]
  }

  return(results)
}

#' Apply BITOLA-style discovery model
#'
#' This function implements a BITOLA-style discovery model based on MeSH term
#' co-occurrence and semantic type filtering.
#'
#' @param co_matrix A co-occurrence matrix produced by create_cooccurrence_matrix().
#' @param a_term Character string, the source term (A).
#' @param a_semantic_type Character string, the semantic type for A term.
#' @param c_semantic_type Character string, the semantic type for C terms.
#' @param min_score Minimum score threshold for results.
#' @param n_results Maximum number of results to return.
#'
#' @return A data frame with ranked discovery results.
#' @export
#'
#' @examples
#' \dontrun{
#' bitola_results <- bitola_model(co_matrix, a_term = "migraine",
#'                                    a_semantic_type = "Disease",
#'                                    c_semantic_type = "Gene")
#' }
bitola_model <- function(co_matrix, a_term, a_semantic_type = NULL,
                         c_semantic_type = NULL, min_score = 0.1,
                         n_results = 100) {

  # Check if semantic types are provided
  if (is.null(a_semantic_type) || is.null(c_semantic_type)) {
    stop("Both A and C semantic types must be provided for BITOLA model")
  }

  # Check if the matrix has entity types
  has_entity_types <- !is.null(attr(co_matrix, "entity_types"))

  if (!has_entity_types) {
    stop("Entity types must be available in the co-occurrence matrix for BITOLA model")
  }

  # Check if A term exists in the matrix
  if (!a_term %in% rownames(co_matrix)) {
    stop("A-term '", a_term, "' not found in the co-occurrence matrix")
  }

  # Get entity types
  entity_types <- attr(co_matrix, "entity_types")

  # Check if A term is of the specified semantic type
  if (entity_types[a_term] != a_semantic_type) {
    stop("A-term '", a_term, "' is not of semantic type '", a_semantic_type, "'")
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
      a_type = character(),
      b_term = character(),
      b_type = character(),
      c_term = character(),
      c_type = character(),
      a_b_score = numeric(),
      b_c_score = numeric(),
      bitola_score = numeric(),
      support = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Get potential C terms of the specified semantic type
  c_type_terms <- names(entity_types[entity_types == c_semantic_type])

  # Initialize results
  results <- data.frame(
    a_term = character(),
    a_type = character(),
    b_term = character(),
    b_type = character(),
    c_term = character(),
    c_type = character(),
    a_b_score = numeric(),
    b_c_score = numeric(),
    bitola_score = numeric(),
    support = integer(),
    stringsAsFactors = FALSE
  )

  # For each B term
  message("Analyzing ", length(b_terms), " B terms...")
  pb <- utils::txtProgressBar(min = 0, max = length(b_terms), style = 3)

  for (b_idx in seq_along(b_terms)) {
    utils::setTxtProgressBar(pb, b_idx)

    b_term <- b_terms[b_idx]
    b_type <- entity_types[b_term]

    # Get B-C associations
    b_associations <- co_matrix[b_term, ]

    # Filter C terms with sufficient connection
    potential_c_for_b <- names(b_associations[b_associations > min_score])

    # Intersect with C terms of the specified semantic type
    potential_c_for_b <- intersect(potential_c_for_b, c_type_terms)

    if (length(potential_c_for_b) > 0) {
      for (c_term in potential_c_for_b) {
        # Get scores
        a_b_score <- a_associations[b_term]
        b_c_score <- b_associations[c_term]

        # Calculate BITOLA score
        # Modified formula based on BITOLA paper
        bitola_score <- (a_b_score * b_c_score)^2

        # Create result entry
        results <- rbind(results, data.frame(
          a_term = a_term,
          a_type = a_semantic_type,
          b_term = b_term,
          b_type = b_type,
          c_term = c_term,
          c_type = c_semantic_type,
          a_b_score = a_b_score,
          b_c_score = b_c_score,
          bitola_score = bitola_score,
          support = 1,  # Will be summed later
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  close(pb)

  # If no results found, return empty data frame
  if (nrow(results) == 0) {
    message("No BITOLA connections found")
    return(results)
  }

  # Aggregate results by C term (support count)
  aggregated <- aggregate(
    cbind(support, bitola_score) ~ a_term + a_type + c_term + c_type,
    data = results,
    FUN = function(x) c(sum = sum(x), max = max(x))
  )

  # Restructure the aggregated results
  final_results <- data.frame(
    a_term = aggregated$a_term,
    a_type = aggregated$a_type,
    c_term = aggregated$c_term,
    c_type = aggregated$c_type,
    support = aggregated$support[, "sum"],
    bitola_score = aggregated$bitola_score[, "max"],
    stringsAsFactors = FALSE
  )

  # Add intermediate B terms for each A-C pair
  final_results$b_terms <- sapply(1:nrow(final_results), function(i) {
    ac_results <- results[results$a_term == final_results$a_term[i] &
                            results$c_term == final_results$c_term[i], ]

    # Sort B terms by score
    sorted_indices <- order(-ac_results$bitola_score)
    sorted_b_terms <- ac_results$b_term[sorted_indices]

    # Return comma-separated list of B terms
    paste(sorted_b_terms, collapse = ", ")
  })

  # Calculate new ranking score based on both support and BITOLA score
  final_results$ranking_score <- final_results$support * final_results$bitola_score

  # Sort by ranking score and limit to n_results
  final_results <- final_results[order(-final_results$ranking_score), ]
  if (nrow(final_results) > n_results) {
    final_results <- final_results[1:n_results, ]
  }

  return(final_results)
}

#' LSI model with enhanced biomedical term filtering and NLP verification
#'
#' This function implements an improved LSI model that more rigorously filters out
#' non-biomedical terms from the results to ensure clinical relevance.
#'
#' @param term_doc_matrix A term-document matrix.
#' @param a_term Character string, the source term (A).
#' @param n_factors Number of factors to use in LSI.
#' @param n_results Maximum number of results to return.
#' @param enforce_biomedical_terms Logical. If TRUE, enforces strict biomedical term filtering.
#' @param c_term_types Character vector of entity types allowed for C terms.
#' @param entity_types Named vector of entity types (if NULL, will try to detect).
#' @param validation_function Function to validate biomedical terms.
#' @param min_word_length Minimum word length to include.
#' @param use_nlp Logical. If TRUE, uses NLP-based validation for biomedical terms.
#' @param nlp_threshold Numeric between 0 and 1. Minimum confidence for NLP validation.
#'
#' @return A data frame with ranked discovery results.
#' @export
lsi_model <- function(term_doc_matrix, a_term, n_factors = 100, n_results = 100,
                      enforce_biomedical_terms = TRUE,
                      c_term_types = NULL,
                      entity_types = NULL,
                      validation_function = is_valid_biomedical_entity,
                      min_word_length = 3,
                      use_nlp = TRUE,
                      nlp_threshold = 0.7) {

  # Check if SVD package is available
  if (!requireNamespace("irlba", quietly = TRUE)) {
    stop("The irlba package is required for LSI. Install it with: install.packages('irlba')")
  }

  # Check if A term exists in the matrix
  if (!a_term %in% rownames(term_doc_matrix)) {
    stop("A-term '", a_term, "' not found in the term-document matrix")
  }

  # Number of factors should not exceed rank limits
  n_factors <- min(n_factors, min(nrow(term_doc_matrix), ncol(term_doc_matrix)) - 1)

  # Perform TF-IDF weighting
  # Term frequency (already in the matrix)
  # Document frequency
  doc_freq <- rowSums(term_doc_matrix > 0)

  # Inverse document frequency
  idf <- log(ncol(term_doc_matrix) / doc_freq)

  # Apply IDF weighting
  tfidf_matrix <- term_doc_matrix * idf

  # Perform SVD using irlba for efficiency with large matrices
  message("Performing SVD with ", n_factors, " factors...")
  svd_result <- irlba::irlba(tfidf_matrix, nv = n_factors)

  # Calculate term and document vectors in latent space
  term_vectors <- svd_result$u %*% diag(svd_result$d)

  # Get index of A term
  a_idx <- which(rownames(term_doc_matrix) == a_term)

  # Get the A term vector in latent space
  a_vector <- term_vectors[a_idx, ]

  # Calculate cosine similarity between A term and all other terms
  # Normalize vectors
  term_vectors_norm <- term_vectors / sqrt(rowSums(term_vectors^2))
  a_vector_norm <- a_vector / sqrt(sum(a_vector^2))

  # Calculate similarities
  similarities <- term_vectors_norm %*% a_vector_norm

  # Remove A term from results
  similarities[a_idx] <- -1

  # Get all term indices
  all_indices <- 1:length(similarities)

  # Apply biomedical term filtering if requested
  if (enforce_biomedical_terms) {
    # Filter out terms with length less than min_word_length
    short_term_indices <- which(nchar(rownames(term_doc_matrix)) < min_word_length)

    # Filter out blacklisted terms (case-insensitive) (using static_data)
    blacklist_indices <- which(tolower(rownames(term_doc_matrix)) %in% static_data$blacklisted_terms)

    # Combine all indices to exclude
    exclude_indices <- unique(c(a_idx, short_term_indices, blacklist_indices))

    # Get remaining indices
    candidate_indices <- setdiff(all_indices, exclude_indices)

    # Validate biomedical relevance of remaining terms
    if (!is.null(validation_function)) {
      message("Validating biomedical relevance of terms...")

      valid_indices <- integer(0)
      candidate_terms <- rownames(term_doc_matrix)[candidate_indices]

      # New scoring system for biomedical relevance
      biomedical_scores <- numeric(length(candidate_indices))

      for (i in seq_along(candidate_indices)) {
        term <- candidate_terms[i]

        # Get entity type if available
        term_type <- NULL
        if (!is.null(entity_types) && term %in% names(entity_types)) {
          term_type <- entity_types[term]
        }

        # Apply base validation using provided function (boolean)
        base_valid <- validation_function(term, term_type)

        # NLP validation if requested
        nlp_valid <- FALSE
        if (use_nlp) {
          nlp_valid <- tryCatch({
            # Try to apply NLP-based entity recognition
            if (exists("validate_entity_with_nlp", mode="function")) {
              validate_entity_with_nlp(term, term_type)
            } else {
              # Fallback to base validation if NLP function doesn't exist
              base_valid
            }
          }, error = function(e) {
            message("NLP validation failed for term: ", term, ". Error: ", e$message)
            return(FALSE)
          })
        }

        # Calculate biomedical relevance score based on multiple criteria
        term_score <- 0

        # Add points for passing base validation
        if (base_valid) term_score <- term_score + 0.5

        # Add points for passing NLP validation
        if (nlp_valid) term_score <- term_score + 0.5

        # Check for known biomedical entity patterns (using static_data)
        term_lower <- tolower(term)

        # Check for pattern matches using static_data
        for (category in names(static_data$biomedical_patterns)) {
          pattern <- static_data$biomedical_patterns[[category]]
          if (grepl(pattern, term_lower)) {
            term_score <- term_score + 0.25  # Add points for pattern matches
            break  # Only count one match per category
          }
        }

        # Store the score
        biomedical_scores[i] <- term_score
      }

      # Select terms with score above threshold
      valid_indices <- candidate_indices[biomedical_scores >= nlp_threshold]

      message("Retained ", length(valid_indices), " biomedically relevant terms after validation")
    }

    # Update indices for ranking
    all_indices <- valid_indices
  }

  # Get top similar terms from filtered set
  top_indices <- all_indices[order(similarities[all_indices], decreasing = TRUE)]
  top_indices <- head(top_indices, min(n_results, length(top_indices)))

  # Extract final results
  top_terms <- rownames(term_doc_matrix)[top_indices]
  top_scores <- similarities[top_indices]

  # Create result data frame
  results <- data.frame(
    a_term = rep(a_term, length(top_terms)),
    c_term = top_terms,
    lsi_similarity = top_scores,
    stringsAsFactors = FALSE
  )

  # Add entity type information if available
  if (!is.null(entity_types)) {
    results$c_type <- sapply(results$c_term, function(term) {
      if (term %in% names(entity_types)) entity_types[term] else NA
    })

    # If c_term_types is specified, filter results accordingly
    if (!is.null(c_term_types) && length(c_term_types) > 0) {
      results <- results[results$c_type %in% c_term_types | is.na(results$c_type), ]
    }
  }

  return(results)
}

#' Create a term-document matrix from preprocessed text
#'
#' This function creates a term-document matrix from preprocessed text data.
#'
#' @param preprocessed_data A data frame with preprocessed text data.
#' @param min_df Minimum document frequency for a term to be included.
#' @param max_df Maximum document frequency (as a proportion) for a term to be included.
#'
#' @return A term-document matrix.
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessed <- preprocess_text(article_data, text_column = "abstract")
#' tdm <- create_tdm(preprocessed)
#' }
create_tdm <- function(preprocessed_data, min_df = 2, max_df = 0.9) {

  # Check if terms column exists
  if (!"terms" %in% colnames(preprocessed_data)) {
    stop("Terms column not found in preprocessed data")
  }

  # Extract all unique terms
  all_terms <- unique(unlist(lapply(preprocessed_data$terms, function(df) {
    if (is.data.frame(df) && nrow(df) > 0) {
      return(df$word)
    } else {
      return(character(0))
    }
  })))

  # Create term-document matrix
  tdm <- matrix(0, nrow = length(all_terms), ncol = nrow(preprocessed_data))
  rownames(tdm) <- all_terms

  # Fill the term-document matrix
  for (i in 1:nrow(preprocessed_data)) {
    terms_df <- preprocessed_data$terms[[i]]
    if (is.data.frame(terms_df) && nrow(terms_df) > 0) {
      for (j in 1:nrow(terms_df)) {
        term <- terms_df$word[j]
        count <- terms_df$count[j]
        tdm[term, i] <- count
      }
    }
  }

  # Calculate document frequency
  doc_freq <- rowSums(tdm > 0)

  # Filter terms by document frequency
  min_doc_count <- min_df
  max_doc_count <- max_df * ncol(tdm)

  valid_terms <- which(doc_freq >= min_doc_count & doc_freq <= max_doc_count)

  if (length(valid_terms) == 0) {
    stop("No terms remain after filtering by document frequency")
  }

  # Subset matrix to include only valid terms
  tdm <- tdm[valid_terms, , drop = FALSE]

  return(tdm)
}
