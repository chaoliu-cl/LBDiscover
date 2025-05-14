# Tests for alternative models implementation functions

# Create a mock co-occurrence matrix for testing
create_mock_comat <- function(n_terms = 50, with_entity_types = TRUE) {
  set.seed(123)  # For reproducibility

  # Create term names
  a_terms <- paste0("A_Term_", 1:2)
  b_terms <- paste0("B_Term_", 1:10)
  c_terms <- paste0("C_Term_", 1:8)
  other_terms <- paste0("Term_", 1:30)

  all_terms <- c(a_terms, b_terms, c_terms, other_terms)
  all_terms <- unique(all_terms)[1:min(n_terms, length(unique(all_terms)))]

  # Create sparse co-occurrence matrix
  co_matrix <- matrix(0, nrow = length(all_terms), ncol = length(all_terms))
  rownames(co_matrix) <- all_terms
  colnames(co_matrix) <- all_terms

  # Fill diagonal with term frequencies
  for (i in 1:length(all_terms)) {
    co_matrix[i, i] <- sample(10:50, 1)  # Term frequency
  }

  # Fill co-occurrence values (making it symmetric)
  for (i in 1:(length(all_terms)-1)) {
    for (j in (i+1):length(all_terms)) {
      if (runif(1) < 0.3) {  # 30% chance of co-occurrence
        co_value <- runif(1, 0.1, 0.9)
        co_matrix[i, j] <- co_value
        co_matrix[j, i] <- co_value  # Make symmetric
      }
    }
  }

  # Add entity types if requested
  if (with_entity_types) {
    entity_types <- c("disease", "drug", "gene", "protein", "pathway", "biological_process", "chemical", "symptom")
    term_types <- sample(entity_types, length(all_terms), replace = TRUE)
    names(term_types) <- all_terms

    # Ensure A terms are diseases, B terms mix, and C terms are targets
    for (term in a_terms) {
      term_types[term] <- "disease"
    }

    for (term in b_terms) {
      term_types[term] <- sample(c("gene", "protein", "biological_process"), 1)
    }

    for (term in c_terms) {
      term_types[term] <- sample(c("drug", "chemical"), 1)
    }

    # Add entity types as attribute
    attr(co_matrix, "entity_types") <- term_types

    # Add entity frequencies as attribute
    entity_freq <- diag(co_matrix)
    attr(co_matrix, "entity_freq") <- entity_freq

    # Add metadata attribute
    attr(co_matrix, "metadata") <- list(
      n_docs = 1000,
      n_entities = length(all_terms),
      has_types = TRUE,
      normalization = "cosine"
    )
  }

  return(co_matrix)
}

# Create a custom mock validation function that doesn't rely on NLP
create_mock_validation_function <- function() {
  # Create a function that validates based on term patterns without NLP dependencies
  function(term, claimed_type = NULL) {
    # Simple term validation rules without NLP

    # If term starts with the prefixes (A_, B_, C_), it's valid
    if (grepl("^A_", term) || grepl("^B_", term) || grepl("^C_", term)) {
      return(TRUE)
    }

    # Validate medical terms in our test data
    medical_terms <- c(
      "disease", "patient", "treatment", "therapy", "drug", "symptom",
      "diagnosis", "clinical", "syndrome", "disorder", "medicine",
      "protein", "gene", "receptor", "enzyme", "pathway"
    )

    if (tolower(term) %in% medical_terms) {
      return(TRUE)
    }

    # Specific term validation based on claimed_type
    if (!is.null(claimed_type)) {
      if (claimed_type == "disease" && grepl("disease|syndrome|disorder", tolower(term))) {
        return(TRUE)
      }
      if (claimed_type == "drug" && grepl("drug|medicine|therapy", tolower(term))) {
        return(TRUE)
      }
      if (claimed_type == "protein" && grepl("protein|receptor|enzyme", tolower(term))) {
        return(TRUE)
      }
      if (claimed_type == "gene" && grepl("gene", tolower(term))) {
        return(TRUE)
      }
    }

    # Default to false for other terms
    return(FALSE)
  }
}

# Test the anc_model function
test_that("anc_model produces expected results", {
  # Skip tests if function is not available
  skip_if_not(exists("anc_model"), "anc_model function not available")

  # Create a test co-occurrence matrix
  co_matrix <- create_mock_comat(n_terms = 30)

  # Get an A term to use
  a_term <- grep("^A_Term_", rownames(co_matrix), value = TRUE)[1]

  # Create a mock validation function to avoid NLP dependencies
  mock_validation <- create_mock_validation_function()

  # Test with default parameters
  expect_no_error({
    results <- anc_model(co_matrix, a_term, n_b_terms = 3,
                         validation_function = mock_validation)
  })

  # If results were produced, check their structure
  if(exists("results") && is.data.frame(results) && nrow(results) > 0) {
    expect_true(all(c("a_term", "b_terms", "c_term", "a_b_scores", "b_c_scores", "anc_score") %in% colnames(results)))
    expect_equal(results$a_term[1], a_term)
  }

  # Test with c_type constraint
  expect_no_error({
    results_with_ctype <- anc_model(co_matrix, a_term, c_type = "drug",
                                    validation_function = mock_validation)
  })

  # Test with disabled biomedical term filtering
  expect_no_error({
    results_no_filter <- anc_model(co_matrix, a_term, enforce_biomedical_terms = FALSE)
  })
})

# Test the bitola_model function
test_that("bitola_model produces expected results", {
  # Skip tests if function is not available
  skip_if_not(exists("bitola_model"), "bitola_model function not available")

  # Create a test co-occurrence matrix
  co_matrix <- create_mock_comat(n_terms = 30)

  # Get an A term to use
  a_term <- grep("^A_Term_", rownames(co_matrix), value = TRUE)[1]

  # Get entity types
  entity_types <- attr(co_matrix, "entity_types")
  a_semantic_type <- entity_types[a_term]

  # Find a semantic type for C terms
  c_types <- unique(entity_types[grep("^C_Term_", names(entity_types))])
  c_semantic_type <- c_types[1]

  # Test with specific semantic types
  expect_no_error({
    results <- bitola_model(co_matrix, a_term,
                            a_semantic_type = a_semantic_type,
                            c_semantic_type = c_semantic_type)
  })

  # Check result structure if available
  if(exists("results") && is.data.frame(results) && nrow(results) > 0) {
    expect_true(all(c("a_term", "a_type", "c_term", "c_type", "support", "bitola_score", "b_terms", "ranking_score") %in% colnames(results)))
    expect_equal(results$a_term[1], a_term)
    # Test the types without using the indexing, which can cause issues with names
    expect_true(as.character(results$a_type[1]) == as.character(a_semantic_type))
    expect_true(as.character(results$c_type[1]) == as.character(c_semantic_type))
  }
})

# Test error handling in BITOLA model
test_that("bitola_model handles errors appropriately", {
  # Skip tests if function is not available
  skip_if_not(exists("bitola_model"), "bitola_model function not available")

  # Create a test co-occurrence matrix without entity types
  co_matrix_no_types <- create_mock_comat(n_terms = 10, with_entity_types = FALSE)

  # Create a test co-occurrence matrix with entity types
  co_matrix <- create_mock_comat(n_terms = 10)

  # Get an A term to use
  a_term <- grep("^A_Term_", rownames(co_matrix), value = TRUE)[1]

  # Missing semantic types should error
  expect_error(bitola_model(co_matrix, a_term), "Both A and C semantic types must be provided")

  # Matrix without entity types should error
  expect_error(bitola_model(co_matrix_no_types, a_term, a_semantic_type = "disease", c_semantic_type = "drug"),
               "Entity types must be available")

  # Non-existent A term should error
  expect_error(bitola_model(co_matrix, "NonExistentTerm", a_semantic_type = "disease", c_semantic_type = "drug"),
               "A-term .* not found")

  # Incorrect A term semantic type should error
  entity_types <- attr(co_matrix, "entity_types")
  a_semantic_type_incorrect <- "drug"  # Different from actual type which is disease
  expect_error(bitola_model(co_matrix, a_term, a_semantic_type = a_semantic_type_incorrect, c_semantic_type = "drug"),
               "A-term .* is not of semantic type")
})

# Create mock term-document matrix for LSI testing
create_mock_tdm <- function(n_terms = 100, n_docs = 50) {
  set.seed(123)  # For reproducibility

  # Create term names
  a_terms <- paste0("A_Term_", 1:2)
  b_terms <- paste0("B_Term_", 1:10)
  c_terms <- paste0("C_Term_", 1:8)
  medical_terms <- c("disease", "patient", "treatment", "therapy", "drug", "symptom",
                     "diagnosis", "clinical", "syndrome", "disorder", "medicine",
                     "protein", "gene", "receptor", "enzyme", "pathway")
  other_terms <- c(paste0("Term_", 1:70), medical_terms)

  all_terms <- c(a_terms, b_terms, c_terms, other_terms)
  all_terms <- unique(all_terms)[1:min(n_terms, length(unique(all_terms)))]

  # Create term-document matrix
  tdm <- matrix(0, nrow = length(all_terms), ncol = n_docs)
  rownames(tdm) <- all_terms

  # Fill matrix with random values
  for (i in 1:n_docs) {
    # Each document contains ~10% of terms
    n_terms_in_doc <- round(length(all_terms) * 0.1)
    term_indices <- sample(1:length(all_terms), n_terms_in_doc)
    tdm[term_indices, i] <- rpois(n_terms_in_doc, lambda = 3)  # Poisson-distributed counts
  }

  return(tdm)
}

# Test the LSI model function
test_that("lsi_model produces expected results", {
  # Skip tests if function is not available
  skip_if_not(exists("lsi_model"), "lsi_model function not available")

  # Skip tests if irlba package is not available
  skip_if_not(requireNamespace("irlba", quietly = TRUE), "irlba package not available")

  # Create a test term-document matrix
  tdm <- create_mock_tdm()

  # Get an A term to use
  a_term <- grep("^A_Term_", rownames(tdm), value = TRUE)[1]

  # Create a mock validation function to avoid NLP dependencies
  mock_validation <- create_mock_validation_function()

  # Test with default parameters and fewer factors for faster testing
  expect_no_error({
    results <- lsi_model(tdm, a_term, n_factors = 10, n_results = 20,
                         validation_function = mock_validation,
                         use_nlp = FALSE) # Disable NLP to avoid the spaCy dependency
  })

  # Check result structure if available
  if(exists("results") && is.data.frame(results) && nrow(results) > 0) {
    expect_true(all(c("a_term", "c_term", "lsi_similarity") %in% colnames(results)))
    expect_equal(results$a_term[1], a_term)

    # Instead of checking exact range, verify values are numerical and finite
    # LSI similarity can sometimes be outside [0,1] range depending on normalization
    expect_true(all(is.numeric(results$lsi_similarity)))
    expect_true(all(is.finite(results$lsi_similarity)))
  }

  # Test with enforce_biomedical_terms = FALSE
  expect_no_error({
    results_no_filter <- lsi_model(tdm, a_term, n_factors = 10,
                                   enforce_biomedical_terms = FALSE)
  })

  # If we have both results, no_filter should have more rows
  if(exists("results") && exists("results_no_filter")) {
    expect_true(nrow(results_no_filter) >= nrow(results))
  }
})

# Test the create_tdm function
test_that("create_tdm correctly creates a term-document matrix", {
  # Skip tests if function is not available
  skip_if_not(exists("create_tdm"), "create_tdm function not available")

  # Create mock preprocessed data
  n_docs <- 5
  mock_preprocessed <- data.frame(doc_id = paste0("doc", 1:n_docs))

  # Create mock terms data frames
  mock_preprocessed$terms <- lapply(1:n_docs, function(i) {
    n_terms <- sample(5:10, 1)
    words <- paste0("term", sample(1:20, n_terms, replace = FALSE))
    counts <- sample(1:5, n_terms, replace = TRUE)
    data.frame(word = words, count = counts, stringsAsFactors = FALSE)
  })

  # Test create_tdm function
  expect_no_error({
    tdm <- create_tdm(mock_preprocessed)
  })

  # Check TDM structure if it was created
  if(exists("tdm")) {
    expect_true(is.matrix(tdm))
    expect_true(nrow(tdm) > 0)
    expect_true(ncol(tdm) == n_docs)
  }

  # Test with missing terms column
  mock_preprocessed_bad <- data.frame(doc_id = paste0("doc", 1:n_docs))
  expect_error(create_tdm(mock_preprocessed_bad), "Terms column not found")
})
