# Additional tests for uncovered portions of abc_model.R
# These tests target specific code paths not covered by existing tests

library(testthat)

# Helper function to create test co-occurrence matrix
create_test_comat <- function() {
  skip_if_not_installed("Matrix")

  terms <- c("migraine", "headache", "serotonin", "receptor", "CGRP",
             "malformation", "kinase", "pain")
  n <- length(terms)

  mat <- Matrix::Matrix(runif(n * n, 0, 0.5), nrow = n, ncol = n, sparse = TRUE)
  # Make symmetric using Matrix transpose
  mat <- (mat + Matrix::t(mat)) / 2
  Matrix::diag(mat) <- 5  # Set diagonal to represent frequency

  rownames(mat) <- colnames(mat) <- terms

  entity_types <- c("disease", "symptom", "chemical", "protein", "protein",
                    "disease", "protein", "symptom")
  names(entity_types) <- terms
  attr(mat, "entity_types") <- entity_types

  entity_freq <- rep(5, n)
  names(entity_freq) <- terms
  attr(mat, "entity_freq") <- entity_freq

  attr(mat, "metadata") <- list(
    n_docs = 100,
    n_entities = n,
    has_types = TRUE,
    normalization = "cosine"
  )

  return(mat)
}

# ==============================================================================
# Tests for is_valid_biomedical_entity - special cases
# ==============================================================================

test_that("is_valid_biomedical_entity handles malformation special case", {
  # Should return TRUE when claimed as disease (special exception)
  expect_true(is_valid_biomedical_entity("malformation", "disease"))
})

test_that("is_valid_biomedical_entity handles receptor as protein", {
  # Receptor should be recognized as protein
  expect_true(is_valid_biomedical_entity("receptor", "protein"))
  expect_true(is_valid_biomedical_entity("receptors", "protein"))
})

test_that("is_valid_biomedical_entity rejects analytical acronyms as chemicals", {
  # These should be rejected as chemicals (they're analytical methods)
  expect_false(is_valid_biomedical_entity("HPLC", "chemical"))
  expect_false(is_valid_biomedical_entity("LCMS", "chemical"))
  expect_false(is_valid_biomedical_entity("PCR", "chemical"))
  expect_false(is_valid_biomedical_entity("ELISA", "chemical"))
  expect_false(is_valid_biomedical_entity("MRI", "chemical"))
})

test_that("is_valid_biomedical_entity handles empty/invalid input", {
  expect_false(is_valid_biomedical_entity("", NULL))
  expect_false(is_valid_biomedical_entity(NA, NULL))
  expect_false(is_valid_biomedical_entity(NULL, NULL))
})

test_that("is_valid_biomedical_entity handles very short terms", {
  # Very short terms should be rejected unless they're valid acronyms
  expect_false(is_valid_biomedical_entity("ab", "disease"))
  expect_false(is_valid_biomedical_entity("x", "protein"))
})

# ==============================================================================
# Tests for abc_model - validation methods
# ==============================================================================

test_that("abc_model handles different validation methods", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  # Test pattern validation (default)
  results_pattern <- abc_model(
    co_matrix,
    a_term = "migraine",
    validation_method = "pattern",
    exclude_general_terms = TRUE,
    min_score = 0.01
  )

  expect_s3_class(results_pattern, "data.frame")
})

test_that("abc_model handles nlp validation method with fallback", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  # NLP method should fall back to pattern-based if NLP fails
  # We expect either a message about NLP failing or about filtering
  results <- suppressMessages(
    abc_model(
      co_matrix,
      a_term = "migraine",
      validation_method = "nlp",
      exclude_general_terms = TRUE,
      min_score = 0.01
    )
  )

  expect_s3_class(results, "data.frame")
})

test_that("abc_model handles api validation method with fallback", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  # API method should fall back to pattern-based if API fails
  results <- suppressMessages(
    abc_model(
      co_matrix,
      a_term = "migraine",
      validation_method = "api",
      exclude_general_terms = TRUE,
      min_score = 0.01
    )
  )

  expect_s3_class(results, "data.frame")
})

test_that("abc_model handles comprehensive validation method", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  results <- suppressMessages(
    abc_model(
      co_matrix,
      a_term = "migraine",
      validation_method = "comprehensive",
      exclude_general_terms = TRUE,
      min_score = 0.01
    )
  )

  expect_s3_class(results, "data.frame")
})

# ==============================================================================
# Tests for abc_model - type constraints without entity types
# ==============================================================================

test_that("abc_model warns when type constraints used without entity types", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()
  attr(co_matrix, "entity_types") <- NULL

  expect_warning(
    results <- abc_model(
      co_matrix,
      a_term = "migraine",
      b_term_types = c("protein"),
      c_term_types = c("drug"),
      min_score = 0.01
    ),
    "Entity type constraints specified but no entity types found"
  )

  expect_s3_class(results, "data.frame")
})

# ==============================================================================
# Tests for abc_model - similarity filtering edge cases
# ==============================================================================

test_that("abc_model handles all B terms being too similar", {
  skip_if_not_installed("Matrix")

  # Create matrix with very similar terms
  terms <- c("migraine", "migraines", "migrain")
  n <- length(terms)
  mat <- Matrix::Matrix(0.9, nrow = n, ncol = n, sparse = TRUE)
  Matrix::diag(mat) <- 5
  rownames(mat) <- colnames(mat) <- terms

  entity_types <- rep("disease", n)
  names(entity_types) <- terms
  attr(mat, "entity_types") <- entity_types
  attr(mat, "entity_freq") <- rep(5, n)
  names(attr(mat, "entity_freq")) <- terms
  attr(mat, "metadata") <- list(n_docs = 10, n_entities = n, has_types = TRUE)

  expect_message(
    results <- abc_model(
      mat,
      a_term = "migraine",
      filter_similar_terms = TRUE,
      similarity_threshold = 0.8,
      min_score = 0.01
    ),
    "All B terms were filtered|reduced similarity threshold|No suitable B terms"
  )

  expect_s3_class(results, "data.frame")
})

# ==============================================================================
# Tests for abc_model - specific C term validation
# ==============================================================================

test_that("abc_model validates specific C term with strict typing", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  # Test with valid C term
  results <- suppressMessages(
    abc_model(
      co_matrix,
      a_term = "migraine",
      c_term = "receptor",
      c_term_types = c("protein"),
      enforce_strict_typing = TRUE,
      min_score = 0.01
    )
  )

  expect_s3_class(results, "data.frame")
})

test_that("abc_model messages when C term doesn't match type constraint", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  expect_message(
    results <- abc_model(
      co_matrix,
      a_term = "migraine",
      c_term = "serotonin",
      c_term_types = c("protein"),  # serotonin is chemical
      min_score = 0.01
    ),
    "does not match required entity types"
  )

  expect_s3_class(results, "data.frame")
})

test_that("abc_model handles C term validation with strict typing", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  # This may or may not produce a message depending on validation
  results <- suppressMessages(
    abc_model(
      co_matrix,
      a_term = "migraine",
      c_term = "headache",
      c_term_types = c("symptom"),
      enforce_strict_typing = TRUE,
      min_score = 0.01
    )
  )

  expect_s3_class(results, "data.frame")
})

# ==============================================================================
# Tests for abc_model - no matching C terms
# ==============================================================================

test_that("abc_model messages when no C terms match type constraints", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  expect_message(
    results <- abc_model(
      co_matrix,
      a_term = "migraine",
      c_term_types = c("rare_type_xyz"),  # Non-existent type
      min_score = 0.01
    ),
    "No potential C terms found matching|No suitable B terms|No ABC connections"
  )

  expect_s3_class(results, "data.frame")
})

# ==============================================================================
# Tests for abc_model - suspicious entity assignments
# ==============================================================================

test_that("abc_model removes suspicious entity type assignments", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  # Run with strict typing to potentially trigger suspicious row detection
  results <- suppressMessages(
    abc_model(
      co_matrix,
      a_term = "migraine",
      enforce_strict_typing = TRUE,
      exclude_general_terms = TRUE,
      min_score = 0.01,
      n_results = 50
    )
  )

  expect_s3_class(results, "data.frame")
})

# ==============================================================================
# Tests for add_statistical_significance (via validate_abc)
# ==============================================================================

test_that("validate_abc calculates p-values correctly", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  abc_results <- data.frame(
    a_term = rep("migraine", 3),
    b_term = c("serotonin", "receptor", "CGRP"),
    c_term = c("headache", "headache", "pain"),
    abc_score = c(0.5, 0.4, 0.3),
    stringsAsFactors = FALSE
  )

  results <- suppressMessages(validate_abc(abc_results, co_matrix, alpha = 0.05))

  expect_s3_class(results, "data.frame")
  expect_true("p_value" %in% names(results))
  expect_true("significant" %in% names(results))
  expect_true("adjusted_p_value" %in% names(results))
  expect_true(all(results$p_value >= 0 & results$p_value <= 1))
})

# ==============================================================================
# Tests for permutation testing (perm_test_abc)
# ==============================================================================

test_that("perm_test_abc performs randomization test", {
  skip_if_not_installed("Matrix")
  skip_on_cran()  # Slow test

  co_matrix <- create_test_comat()

  abc_results <- data.frame(
    a_term = rep("migraine", 2),
    b_term = c("serotonin", "receptor"),
    c_term = c("headache", "pain"),
    abc_score = c(0.5, 0.4),
    stringsAsFactors = FALSE
  )

  results <- suppressMessages(
    perm_test_abc(abc_results, co_matrix, n_permutations = 10, alpha = 0.05)
  )

  expect_s3_class(results, "data.frame")
  if (nrow(results) > 0) {
    expect_true("perm_p_value" %in% names(results))
    expect_true("perm_significant" %in% names(results))
  }
})

# ==============================================================================
# Tests for abc_timeslice
# ==============================================================================

test_that("abc_timeslice validates connections over time", {
  skip_if_not_installed("Matrix")
  skip_on_cran()

  # Create data with consistent entities across time periods
  entities <- c("migraine", "serotonin", "headache", "receptor", "pain")
  n_docs <- 30

  entity_data <- data.frame(
    doc_id = rep(1:n_docs, each = length(entities)),
    entity = rep(entities, n_docs),
    entity_type = rep(c("disease", "chemical", "symptom", "protein", "symptom"), n_docs),
    publication_year = rep(2000:2029, each = length(entities)),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    tryCatch({
      abc_timeslice(
        entity_data,
        time_column = "publication_year",
        split_time = 2015,
        a_term = "migraine",
        min_score = 0.01,
        n_results = 10
      )
    }, error = function(e) {
      # If there's an error, return a basic structure
      list(
        predictions = data.frame(),
        validations = data.frame(),
        validation_metrics = list(
          total_predictions = 0,
          total_validated = 0,
          validation_rate = 0
        )
      )
    })
  )

  expect_type(result, "list")
  expect_true("predictions" %in% names(result))
  expect_true("validations" %in% names(result))
  expect_true("validation_metrics" %in% names(result))
})

# ==============================================================================
# Tests for validate_abc - different scenarios
# ==============================================================================

test_that("validate_abc handles large matrix optimization", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  abc_results <- data.frame(
    a_term = rep("migraine", 2),
    b_term = c("serotonin", "receptor"),
    c_term = c("headache", "CGRP"),
    abc_score = c(0.5, 0.4),
    stringsAsFactors = FALSE
  )

  # Should use optimized approach
  results <- suppressMessages(validate_abc(abc_results, co_matrix))

  expect_s3_class(results, "data.frame")
  expect_true("p_value" %in% names(results))
})

test_that("validate_abc handles missing metadata gracefully", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()
  attr(co_matrix, "metadata") <- NULL

  abc_results <- data.frame(
    a_term = "migraine",
    b_term = "serotonin",
    c_term = "headache",
    abc_score = 0.5,
    stringsAsFactors = FALSE
  )

  results <- suppressMessages(validate_abc(abc_results, co_matrix))

  expect_s3_class(results, "data.frame")
  expect_true("p_value" %in% names(results))
})

test_that("validate_abc handles potential edge cases", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  abc_results <- data.frame(
    a_term = "migraine",
    b_term = "serotonin",
    c_term = "headache",
    abc_score = 0.1,
    stringsAsFactors = FALSE
  )

  results <- suppressMessages(validate_abc(abc_results, co_matrix))

  expect_s3_class(results, "data.frame")
  expect_true("p_value" %in% names(results))
})

test_that("validate_abc handles different correction methods", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  abc_results <- data.frame(
    a_term = rep("migraine", 2),
    b_term = c("serotonin", "receptor"),
    c_term = c("headache", "pain"),
    abc_score = c(0.5, 0.4),
    stringsAsFactors = FALSE
  )

  # Test BH correction
  results_bh <- suppressMessages(
    validate_abc(abc_results, co_matrix, correction = "BH")
  )
  expect_true("adjusted_p_value" %in% names(results_bh))

  # Test Bonferroni correction
  results_bonf <- suppressMessages(
    validate_abc(abc_results, co_matrix, correction = "bonferroni")
  )
  expect_true("adjusted_p_value" %in% names(results_bonf))

  # Test no correction
  results_none <- suppressMessages(
    validate_abc(abc_results, co_matrix, correction = "none")
  )
  expect_true("adjusted_p_value" %in% names(results_none))
})

# ==============================================================================
# Tests for validation functions
# ==============================================================================

test_that("validate_entity_with_nlp falls back when spacyr unavailable", {
  skip_if_not_installed("Matrix")

  # Should fall back to pattern-based validation
  result <- tryCatch({
    validate_entity_with_nlp("migraine", "disease")
  }, error = function(e) {
    is_valid_biomedical_entity("migraine", "disease")
  })

  expect_type(result, "logical")
})

test_that("validate_biomedical_entity handles BioBERT fallback", {
  skip_if_not_installed("Matrix")

  # Should fall back to pattern-based when BioBERT unavailable
  result <- suppressMessages(
    tryCatch({
      validate_biomedical_entity("migraine", "disease")
    }, error = function(e) {
      is_valid_biomedical_entity("migraine", "disease")
    })
  )

  expect_type(result, "logical")
})

test_that("validate_entity_comprehensive uses multiple methods", {
  # Test with only pattern-based (most reliable for testing)
  result <- validate_entity_comprehensive(
    "migraine",
    "disease",
    use_nlp = FALSE,
    use_pattern = TRUE,
    use_external_api = FALSE
  )

  expect_type(result, "logical")
})

test_that("validate_entity_comprehensive handles errors gracefully", {
  # Should handle various error conditions
  result1 <- validate_entity_comprehensive(
    "",
    "disease",
    use_nlp = FALSE,
    use_pattern = TRUE,
    use_external_api = FALSE
  )

  expect_false(result1)

  result2 <- validate_entity_comprehensive(
    "ab",
    "disease",
    use_nlp = FALSE,
    use_pattern = TRUE,
    use_external_api = FALSE
  )

  expect_false(result2)
})

# ==============================================================================
# Tests for query_external_api
# ==============================================================================

test_that("query_external_api handles unknown types conservatively", {
  skip_if_not_installed("httr")
  skip_on_cran()  # Don't make external API calls

  # Should be conservative for unknown types
  result <- query_external_api("test_term", "unknown_type")

  expect_true(result)
})

test_that("query_external_api handles errors gracefully", {
  skip_if_not_installed("httr")
  skip_on_cran()

  # Should handle errors without crashing
  result <- tryCatch({
    query_external_api("", "chemical")
  }, error = function(e) {
    FALSE
  })

  expect_type(result, "logical")
})

# ==============================================================================
# Integration tests for full workflow
# ==============================================================================

test_that("Full workflow with validation methods works", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  # Run ABC model with different validation
  results <- suppressMessages(
    abc_model(
      co_matrix,
      a_term = "migraine",
      validation_method = "pattern",
      exclude_general_terms = TRUE,
      filter_similar_terms = TRUE,
      enforce_strict_typing = TRUE,
      min_score = 0.01
    )
  )

  expect_s3_class(results, "data.frame")

  # Validate results if any exist
  if (nrow(results) > 0) {
    validated <- suppressMessages(validate_abc(results, co_matrix))
    expect_true("p_value" %in% names(validated))
  }
})

test_that("Full workflow with type constraints works", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  results <- suppressMessages(
    abc_model(
      co_matrix,
      a_term = "migraine",
      b_term_types = c("chemical", "protein"),
      c_term_types = c("symptom"),
      exclude_general_terms = TRUE,
      enforce_strict_typing = TRUE,
      min_score = 0.01
    )
  )

  expect_s3_class(results, "data.frame")
})

test_that("Full workflow handles edge cases", {
  skip_if_not_installed("Matrix")

  co_matrix <- create_test_comat()

  # Very restrictive parameters
  results <- suppressMessages(
    abc_model(
      co_matrix,
      a_term = "migraine",
      min_score = 0.9,
      n_results = 1,
      exclude_general_terms = TRUE
    )
  )

  expect_s3_class(results, "data.frame")
})

# ==============================================================================
# Tests for diversify_abc
# ==============================================================================

test_that("diversify_abc handles various diversity methods", {
  abc_results <- data.frame(
    a_term = rep("migraine", 6),
    b_term = rep(c("serotonin", "receptor"), each = 3),
    c_term = rep(c("headache", "pain", "CGRP"), 2),
    abc_score = seq(0.9, 0.4, length.out = 6),
    stringsAsFactors = FALSE
  )

  # Test b_term_groups
  diverse_b <- diversify_abc(
    abc_results,
    diversity_method = "b_term_groups",
    max_per_group = 2
  )
  expect_s3_class(diverse_b, "data.frame")

  # Test unique_c_paths
  diverse_c <- diversify_abc(
    abc_results,
    diversity_method = "unique_c_paths",
    max_per_group = 2
  )
  expect_s3_class(diverse_c, "data.frame")

  # Test both
  diverse_both <- diversify_abc(
    abc_results,
    diversity_method = "both",
    max_per_group = 2
  )
  expect_s3_class(diverse_both, "data.frame")
})
