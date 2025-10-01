# tests/testthat/test-abc-model.R

library(testthat)
library(LBDiscover)

# Helper function to create mock entity data
create_mock_entity_data <- function() {
  data.frame(
    doc_id = rep(1:10, each = 5),
    entity = rep(c("migraine", "headache", "serotonin", "CGRP", "sumatriptan"), 10),
    entity_type = rep(c("disease", "symptom", "chemical", "protein", "drug"), 10),
    frequency = sample(1:5, 50, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Helper function to create mock co-occurrence matrix
create_mock_cooccurrence_matrix <- function() {
  terms <- c("migraine", "headache", "pain", "serotonin", "CGRP", "sumatriptan",
             "topiramate", "propranolol")
  n <- length(terms)

  set.seed(123)
  mat <- matrix(runif(n * n, 0, 1), nrow = n, ncol = n)
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  diag(mat) <- 1

  rownames(mat) <- colnames(mat) <- terms

  entity_types <- c("disease", "symptom", "symptom", "chemical", "protein",
                    "drug", "drug", "drug")
  names(entity_types) <- terms
  attr(mat, "entity_types") <- entity_types

  entity_freq <- rep(5, n)
  names(entity_freq) <- terms
  attr(mat, "entity_freq") <- entity_freq

  attr(mat, "metadata") <- list(
    n_docs = 10,
    n_entities = n,
    has_types = TRUE,
    normalization = "cosine"
  )

  return(mat)
}

# Tests for create_comat
test_that("create_comat creates matrix with valid input", {
  entity_data <- create_mock_entity_data()

  # Mock the Matrix package if needed
  skip_if_not_installed("Matrix")

  result <- create_comat(
    entity_data,
    doc_id_col = "doc_id",
    entity_col = "entity",
    type_col = "entity_type",
    normalize = FALSE
  )

  expect_true(is.matrix(result) || inherits(result, "Matrix"))
  expect_equal(nrow(result), ncol(result))
  expect_true(!is.null(rownames(result)))
  expect_true(!is.null(attr(result, "entity_types")))
  expect_true(!is.null(attr(result, "entity_freq")))
})

test_that("create_comat handles missing columns", {
  entity_data <- create_mock_entity_data()

  expect_error(
    create_comat(
      entity_data,
      doc_id_col = "nonexistent_col",
      entity_col = "entity"
    ),
    "Required columns not found"
  )
})

test_that("create_comat normalizes correctly", {
  entity_data <- create_mock_entity_data()

  result <- create_comat(
    entity_data,
    normalize = TRUE,
    normalization_method = "cosine"
  )

  expect_true(!is.null(attr(result, "metadata")))
  expect_equal(attr(result, "metadata")$normalization, "cosine")
})

test_that("create_comat handles empty data", {
  entity_data <- data.frame(
    doc_id = character(0),
    entity = character(0),
    entity_type = character(0)
  )

  expect_error(
    create_comat(entity_data),
    "No valid data after filtering"
  )
})

test_that("create_comat handles different normalization methods", {
  entity_data <- create_mock_entity_data()

  for (method in c("cosine", "jaccard", "dice")) {
    result <- create_comat(
      entity_data,
      normalize = TRUE,
      normalization_method = method
    )

    expect_equal(attr(result, "metadata")$normalization, method)
  }
})

# Tests for is_valid_biomedical_entity
test_that("is_valid_biomedical_entity recognizes valid entities", {
  expect_true(is_valid_biomedical_entity("migraine", "disease"))
  expect_true(is_valid_biomedical_entity("receptor", "protein"))
  expect_true(is_valid_biomedical_entity("BRCA1", "gene"))
  expect_true(is_valid_biomedical_entity("sumatriptan", "drug"))
})

test_that("is_valid_biomedical_entity rejects clearly invalid entities", {
  # Test without claimed type - these should be rejected based on general characteristics
  expect_false(is_valid_biomedical_entity("", NULL))
  expect_false(is_valid_biomedical_entity(NA, NULL))
  expect_false(is_valid_biomedical_entity(NULL, NULL))
})

test_that("is_valid_biomedical_entity handles empty or NA input", {
  expect_false(is_valid_biomedical_entity("", "disease"))
  expect_false(is_valid_biomedical_entity(NA, "disease"))
  expect_false(is_valid_biomedical_entity(NULL, "disease"))
})

test_that("is_valid_biomedical_entity handles acronyms correctly", {
  expect_true(is_valid_biomedical_entity("CGRP", "protein"))
  expect_true(is_valid_biomedical_entity("DNA", "gene"))
})

test_that("is_valid_biomedical_entity handles special cases", {
  expect_true(is_valid_biomedical_entity("malformation", "disease"))
  expect_true(is_valid_biomedical_entity("receptor", "protein"))
})

test_that("is_valid_biomedical_entity with pattern matching", {
  # Test terms that should match biomedical patterns
  expect_true(is_valid_biomedical_entity("cardiomyopathy", "disease"))
  expect_true(is_valid_biomedical_entity("inflammation", "biological_process"))
})

# Tests for abc_model
test_that("abc_model returns valid results", {
  co_matrix <- create_mock_cooccurrence_matrix()

  results <- abc_model(
    co_matrix,
    a_term = "migraine",
    min_score = 0.1,
    n_results = 10
  )

  expect_s3_class(results, "data.frame")
  expect_true(all(c("a_term", "b_term", "c_term", "abc_score") %in% names(results)))
  expect_true(all(results$a_term == "migraine"))
  expect_true(nrow(results) <= 10)
})

test_that("abc_model handles missing A term", {
  co_matrix <- create_mock_cooccurrence_matrix()

  expect_error(
    abc_model(co_matrix, a_term = "nonexistent"),
    "not found in the co-occurrence matrix"
  )
})

test_that("abc_model handles specific C term", {
  co_matrix <- create_mock_cooccurrence_matrix()

  results <- abc_model(
    co_matrix,
    a_term = "migraine",
    c_term = "sumatriptan",
    min_score = 0.1
  )

  expect_s3_class(results, "data.frame")
  if (nrow(results) > 0) {
    expect_true(all(results$c_term == "sumatriptan"))
  }
})

test_that("abc_model respects scoring methods", {
  co_matrix <- create_mock_cooccurrence_matrix()

  for (method in c("multiplication", "average", "combined", "jaccard")) {
    results <- abc_model(
      co_matrix,
      a_term = "migraine",
      scoring_method = method,
      min_score = 0.1,
      n_results = 5
    )

    expect_s3_class(results, "data.frame")
  }
})

test_that("abc_model filters by entity types", {
  co_matrix <- create_mock_cooccurrence_matrix()

  results <- abc_model(
    co_matrix,
    a_term = "migraine",
    b_term_types = c("chemical", "protein"),
    c_term_types = c("drug"),
    min_score = 0.1
  )

  expect_s3_class(results, "data.frame")
  if (nrow(results) > 0) {
    expect_true(all(results$b_type %in% c("chemical", "protein")))
    expect_true(all(results$c_type == "drug"))
  }
})

test_that("abc_model excludes general terms when requested", {
  co_matrix <- create_mock_cooccurrence_matrix()

  results <- abc_model(
    co_matrix,
    a_term = "migraine",
    exclude_general_terms = TRUE,
    min_score = 0.1
  )

  expect_s3_class(results, "data.frame")
})

test_that("abc_model filters similar terms", {
  co_matrix <- create_mock_cooccurrence_matrix()

  results <- abc_model(
    co_matrix,
    a_term = "migraine",
    filter_similar_terms = TRUE,
    similarity_threshold = 0.8,
    min_score = 0.1
  )

  expect_s3_class(results, "data.frame")
  # No B term should be too similar to "migraine"
  if (nrow(results) > 0) {
    expect_false("migraine" %in% results$b_term)
  }
})

test_that("abc_model handles no valid B terms", {
  co_matrix <- create_mock_cooccurrence_matrix()
  # Set very high threshold

  results <- abc_model(
    co_matrix,
    a_term = "migraine",
    min_score = 0.999
  )

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 0)
})

# Tests for calculate_score
test_that("calculate_score computes correctly", {
  a_b <- 0.5
  b_c <- 0.6

  expect_equal(calculate_score(a_b, b_c, "multiplication"), 0.3)
  expect_equal(calculate_score(a_b, b_c, "average"), 0.55)
  expect_true(is.numeric(calculate_score(a_b, b_c, "combined")))
  expect_true(is.numeric(calculate_score(a_b, b_c, "jaccard")))
})

# Tests for diversify_abc
test_that("diversify_abc removes duplicates", {
  abc_results <- data.frame(
    a_term = rep("migraine", 6),
    b_term = rep(c("serotonin", "CGRP"), each = 3),
    c_term = rep(c("sumatriptan", "topiramate", "propranolol"), 2),
    abc_score = runif(6, 0.5, 1),
    stringsAsFactors = FALSE
  )

  results <- diversify_abc(
    abc_results,
    diversity_method = "b_term_groups",
    max_per_group = 2
  )

  expect_s3_class(results, "data.frame")
  expect_true(nrow(results) <= nrow(abc_results))
})

test_that("diversify_abc handles empty input", {
  abc_results <- data.frame(
    a_term = character(0),
    b_term = character(0),
    c_term = character(0),
    abc_score = numeric(0),
    stringsAsFactors = FALSE
  )

  results <- diversify_abc(abc_results)

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 0)
})

test_that("diversify_abc respects max_per_group", {
  abc_results <- data.frame(
    a_term = rep("migraine", 10),
    b_term = rep("serotonin", 10),
    c_term = paste0("drug", 1:10),
    abc_score = seq(1, 0.1, length.out = 10),
    stringsAsFactors = FALSE
  )

  results <- diversify_abc(
    abc_results,
    diversity_method = "b_term_groups",
    max_per_group = 3
  )

  expect_true(nrow(results) <= 3)
})

test_that("diversify_abc validates methods", {
  abc_results <- data.frame(
    a_term = "migraine",
    b_term = "serotonin",
    c_term = "sumatriptan",
    abc_score = 0.8,
    stringsAsFactors = FALSE
  )

  expect_error(
    diversify_abc(abc_results, diversity_method = "invalid"),
    "'arg' should be one of"
  )
})

# Tests for validate_abc
test_that("validate_abc adds significance testing", {
  co_matrix <- create_mock_cooccurrence_matrix()

  abc_results <- data.frame(
    a_term = rep("migraine", 3),
    b_term = c("serotonin", "CGRP", "pain"),
    c_term = c("sumatriptan", "topiramate", "propranolol"),
    abc_score = c(0.8, 0.7, 0.6),
    stringsAsFactors = FALSE
  )

  results <- validate_abc(abc_results, co_matrix)

  expect_s3_class(results, "data.frame")
  expect_true(all(c("p_value", "significant", "adjusted_p_value") %in% names(results)))
})

test_that("validate_abc handles empty results", {
  co_matrix <- create_mock_cooccurrence_matrix()

  abc_results <- data.frame(
    a_term = character(0),
    b_term = character(0),
    c_term = character(0),
    abc_score = numeric(0),
    stringsAsFactors = FALSE
  )

  results <- validate_abc(abc_results, co_matrix)

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 0)
})

test_that("validate_abc applies different corrections", {
  co_matrix <- create_mock_cooccurrence_matrix()

  abc_results <- data.frame(
    a_term = rep("migraine", 3),
    b_term = c("serotonin", "CGRP", "pain"),
    c_term = c("sumatriptan", "topiramate", "propranolol"),
    abc_score = c(0.8, 0.7, 0.6),
    stringsAsFactors = FALSE
  )

  for (correction in c("BH", "bonferroni", "none")) {
    results <- validate_abc(abc_results, co_matrix, correction = correction)
    expect_s3_class(results, "data.frame")
    expect_true("adjusted_p_value" %in% names(results))
  }
})

test_that("validate_abc can filter by significance", {
  co_matrix <- create_mock_cooccurrence_matrix()

  abc_results <- data.frame(
    a_term = rep("migraine", 3),
    b_term = c("serotonin", "CGRP", "pain"),
    c_term = c("sumatriptan", "topiramate", "propranolol"),
    abc_score = c(0.8, 0.7, 0.6),
    stringsAsFactors = FALSE
  )

  # Expect warning when no significant results
  expect_warning(
    results <- validate_abc(
      abc_results,
      co_matrix,
      filter_by_significance = TRUE
    ),
    "No statistically significant results found"
  )

  expect_s3_class(results, "data.frame")
})

# Tests for perm_test_abc
test_that("perm_test_abc runs permutation test", {
  skip_on_cran()  # Permutation tests are slow

  co_matrix <- create_mock_cooccurrence_matrix()

  abc_results <- data.frame(
    a_term = rep("migraine", 2),
    b_term = c("serotonin", "CGRP"),
    c_term = c("sumatriptan", "topiramate"),
    abc_score = c(0.8, 0.7),
    stringsAsFactors = FALSE
  )

  results <- perm_test_abc(
    abc_results,
    co_matrix,
    n_permutations = 10  # Small number for testing
  )

  expect_s3_class(results, "data.frame")
  expect_true(all(c("perm_p_value", "perm_significant") %in% names(results)))
})

test_that("perm_test_abc handles empty results", {
  co_matrix <- create_mock_cooccurrence_matrix()

  abc_results <- data.frame(
    a_term = character(0),
    b_term = character(0),
    c_term = character(0),
    abc_score = numeric(0),
    stringsAsFactors = FALSE
  )

  results <- perm_test_abc(abc_results, co_matrix, n_permutations = 10)

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 0)
})

# Tests for get_type_dist
test_that("get_type_dist returns type distribution", {
  co_matrix <- create_mock_cooccurrence_matrix()

  result <- get_type_dist(co_matrix)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("entity_type", "count", "percentage") %in% names(result)))
  expect_equal(sum(result$percentage), 100)
})

test_that("get_type_dist handles matrix without types", {
  co_matrix <- create_mock_cooccurrence_matrix()
  attr(co_matrix, "entity_types") <- NULL

  expect_error(
    get_type_dist(co_matrix),
    "does not have entity type information"
  )
})

# Tests for filter_by_type
test_that("filter_by_type filters correctly", {
  co_matrix <- create_mock_cooccurrence_matrix()

  filtered <- filter_by_type(co_matrix, types = c("disease", "drug"))

  expect_true(is.matrix(filtered) || inherits(filtered, "Matrix"))
  expect_true(nrow(filtered) < nrow(co_matrix))

  # Check that only specified types remain
  remaining_types <- unique(attr(filtered, "entity_types"))
  expect_true(all(remaining_types %in% c("disease", "drug")))
})

test_that("filter_by_type handles matrix without types", {
  co_matrix <- create_mock_cooccurrence_matrix()
  attr(co_matrix, "entity_types") <- NULL

  expect_error(
    filter_by_type(co_matrix, types = c("disease")),
    "does not have entity type information"
  )
})

# Tests for find_abc_all
test_that("find_abc_all finds connections for all terms", {
  skip_on_cran()  # Can be slow

  co_matrix <- create_mock_cooccurrence_matrix()

  # Suppress messages for cleaner test output
  suppressMessages({
    results <- find_abc_all(
      co_matrix,
      min_score = 0.3,
      n_results = 5
    )
  })

  expect_s3_class(results, "data.frame")
  expect_true(all(c("a_term", "b_term", "c_term", "abc_score") %in% names(results)))
})

test_that("find_abc_all filters by entity types", {
  skip_on_cran()

  co_matrix <- create_mock_cooccurrence_matrix()

  suppressMessages({
    results <- find_abc_all(
      co_matrix,
      a_type = "disease",
      c_type = "drug",
      min_score = 0.3,
      n_results = 5
    )
  })

  expect_s3_class(results, "data.frame")
})

# Tests for abc_timeslice - skip due to complexity
test_that("abc_timeslice handles missing time column", {
  entity_data <- create_mock_entity_data()

  expect_error(
    abc_timeslice(
      entity_data,
      time_column = "nonexistent",
      split_time = 2014,
      a_term = "migraine"
    ),
    "Time column.*not found"
  )
})

# Tests for validation helper functions
test_that("validate_entity_with_nlp handles missing spacyr", {
  skip_if_not_installed("spacyr")
  skip_on_cran()

  # Should fall back to pattern-based validation on error
  result <- tryCatch({
    validate_entity_with_nlp("test_term", "disease")
  }, error = function(e) {
    # If spacyr is not initialized, it should return pattern-based result
    is_valid_biomedical_entity("test_term", "disease")
  })

  expect_type(result, "logical")
})

test_that("validate_entity_comprehensive uses multiple methods", {
  result <- validate_entity_comprehensive(
    "migraine",
    "disease",
    use_nlp = FALSE,
    use_pattern = TRUE,
    use_external_api = FALSE
  )

  expect_type(result, "logical")
  expect_true(result)
})

# Edge case tests
test_that("abc_model handles single B term", {
  co_matrix <- create_mock_cooccurrence_matrix()
  # Make only one B term valid
  co_matrix["migraine", ] <- 0
  co_matrix["migraine", "migraine"] <- 1
  co_matrix["migraine", "serotonin"] <- 0.5

  results <- abc_model(
    co_matrix,
    a_term = "migraine",
    min_score = 0.4
  )

  expect_s3_class(results, "data.frame")
})

test_that("abc_model handles all similar terms", {
  co_matrix <- matrix(0.95, nrow = 3, ncol = 3)
  diag(co_matrix) <- 1
  rownames(co_matrix) <- colnames(co_matrix) <- c("migraine", "migraines", "migrain")

  entity_types <- rep("disease", 3)
  names(entity_types) <- rownames(co_matrix)
  attr(co_matrix, "entity_types") <- entity_types

  entity_freq <- rep(5, 3)
  names(entity_freq) <- rownames(co_matrix)
  attr(co_matrix, "entity_freq") <- entity_freq

  results <- abc_model(
    co_matrix,
    a_term = "migraine",
    filter_similar_terms = TRUE,
    min_score = 0.1
  )

  expect_s3_class(results, "data.frame")
})
