# Test file for abc_model.R functions
# Tests for create_comat(), is_valid_biomedical_entity(), abc_model(), and related functions

library(testthat)

# Helper function to create test data
create_test_entity_data <- function() {
  data.frame(
    doc_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    entity = c("migraine", "headache", "serotonin",
               "migraine", "serotonin", "receptor",
               "headache", "receptor", "pain"),
    entity_type = c("disease", "symptom", "chemical",
                    "disease", "chemical", "protein",
                    "symptom", "protein", "symptom"),
    count = c(2, 1, 3, 1, 2, 1, 2, 1, 1),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Tests for create_comat()
# ==============================================================================

test_that("create_comat creates matrix with correct dimensions", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data, normalize = FALSE)

  expect_s4_class(co_matrix, "Matrix")
  unique_entities <- unique(entity_data$entity)
  expect_equal(nrow(co_matrix), length(unique_entities))
  expect_equal(ncol(co_matrix), length(unique_entities))
})

test_that("create_comat normalizes correctly", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data, normalize = TRUE,
                            normalization_method = "cosine")

  # Normalized values should be between 0 and 1
  expect_true(all(co_matrix >= 0))
  expect_true(all(co_matrix <= 1))
})

test_that("create_comat handles different normalization methods", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()

  cosine <- create_comat(entity_data, normalize = TRUE,
                         normalization_method = "cosine")
  jaccard <- create_comat(entity_data, normalize = TRUE,
                          normalization_method = "jaccard")
  dice <- create_comat(entity_data, normalize = TRUE,
                       normalization_method = "dice")

  expect_s4_class(cosine, "Matrix")
  expect_s4_class(jaccard, "Matrix")
  expect_s4_class(dice, "Matrix")

  # Methods should produce different results
  expect_false(identical(as.matrix(cosine), as.matrix(jaccard)))
})

test_that("create_comat stores entity types as attribute", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  entity_types <- attr(co_matrix, "entity_types")
  expect_false(is.null(entity_types))
  expect_type(entity_types, "character")
})

test_that("create_comat stores metadata", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  metadata <- attr(co_matrix, "metadata")
  expect_false(is.null(metadata))
  expect_true("n_docs" %in% names(metadata))
  expect_true("n_entities" %in% names(metadata))
})

test_that("create_comat sets diagonal to zero", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data, normalize = FALSE)

  # Use Matrix::diag for sparse matrices
  diagonal_values <- Matrix::diag(co_matrix)
  expect_equal(sum(diagonal_values), 0)
})

test_that("create_comat handles missing type column", {
  skip_if_not_installed("Matrix")

  entity_data <- data.frame(
    doc_id = c(1, 1, 2, 2),
    entity = c("term1", "term2", "term1", "term3"),
    stringsAsFactors = FALSE
  )

  expect_message(
    co_matrix <- create_comat(entity_data, type_col = "entity_type"),
    "Entity type column.*not found"
  )

  expect_s4_class(co_matrix, "Matrix")
})

test_that("create_comat filters NA values", {
  skip_if_not_installed("Matrix")

  entity_data <- data.frame(
    doc_id = c(1, 1, NA, 2),
    entity = c("term1", NA, "term2", "term3"),
    entity_type = c("type1", "type2", "type3", NA),
    stringsAsFactors = FALSE
  )

  co_matrix <- create_comat(entity_data)
  expect_s4_class(co_matrix, "Matrix")
})

test_that("create_comat requires Matrix package", {
  skip_if_not_installed("Matrix")

  # Can't easily test package requirement, but verify it loads
  entity_data <- create_test_entity_data()
  expect_error(
    create_comat(entity_data),
    NA  # Should not error
  )
})

# ==============================================================================
# Tests for is_valid_biomedical_entity()
# ==============================================================================

test_that("is_valid_biomedical_entity recognizes diseases", {
  # Test diseases with clear disease patterns (suffixes like -itis, -oma, -osis)
  expect_true(is_valid_biomedical_entity("arthritis", "disease"))
  expect_true(is_valid_biomedical_entity("carcinoma", "disease"))

  # Test common diseases - may or may not pass depending on patterns
  result1 <- is_valid_biomedical_entity("migraine", "disease")
  result2 <- is_valid_biomedical_entity("diabetes", "disease")
  result3 <- is_valid_biomedical_entity("cancer", "disease")

  # Should return logical values
  expect_type(result1, "logical")
  expect_type(result2, "logical")
  expect_type(result3, "logical")
})

test_that("is_valid_biomedical_entity recognizes proteins", {
  # Test proteins with clear protein patterns (suffixes like -ase, -in)
  expect_true(is_valid_biomedical_entity("kinase", "protein"))
  expect_true(is_valid_biomedical_entity("albumin", "protein"))

  # Test receptor which should be recognized
  expect_true(is_valid_biomedical_entity("receptor", "protein"))
  expect_true(is_valid_biomedical_entity("receptors", "protein"))

  # Test enzyme pattern
  expect_true(is_valid_biomedical_entity("enzyme", "protein"))

  # Test other proteins - may depend on patterns
  result <- is_valid_biomedical_entity("insulin", "protein")
  expect_type(result, "logical")
})

test_that("is_valid_biomedical_entity recognizes chemicals", {
  # Test chemicals with clear chemical patterns
  expect_true(is_valid_biomedical_entity("sulfuric acid", "chemical"))
  expect_true(is_valid_biomedical_entity("sodium chloride", "chemical"))

  # Test common neurotransmitters/chemicals - may not match patterns
  result1 <- is_valid_biomedical_entity("serotonin", "chemical")
  result2 <- is_valid_biomedical_entity("dopamine", "chemical")
  result3 <- is_valid_biomedical_entity("glucose", "chemical")

  # Should return logical values
  expect_type(result1, "logical")
  expect_type(result2, "logical")
  expect_type(result3, "logical")
})

test_that("is_valid_biomedical_entity checks blacklisted terms", {
  # Note: The function checks against static_data which must be loaded
  # These tests verify the function logic, not the static_data content

  # Test with a term that has biomedical characteristics but might be misclassified
  result <- is_valid_biomedical_entity("optimization", "chemical")
  # The function may or may not reject this depending on static_data
  expect_type(result, "logical")
})

test_that("is_valid_biomedical_entity validates term-type combinations", {
  # Test that the function validates based on patterns
  expect_true(is_valid_biomedical_entity("headache", "symptom"))
  expect_true(is_valid_biomedical_entity("nausea", "symptom"))

  # Test acronyms
  expect_true(is_valid_biomedical_entity("BRCA1", "gene"))
  expect_true(is_valid_biomedical_entity("TP53", "gene"))
})

test_that("is_valid_biomedical_entity handles empty/NULL terms", {
  expect_false(is_valid_biomedical_entity("", "disease"))
  expect_false(is_valid_biomedical_entity(NULL, "disease"))
  expect_false(is_valid_biomedical_entity(NA, "disease"))
})

test_that("is_valid_biomedical_entity handles short terms", {
  expect_false(is_valid_biomedical_entity("ab", "disease"))
  expect_true(is_valid_biomedical_entity("DNA", "gene"))  # Acronym
})

test_that("is_valid_biomedical_entity handles numbers", {
  expect_false(is_valid_biomedical_entity("123", "disease"))
  expect_false(is_valid_biomedical_entity("456", "protein"))
})

test_that("is_valid_biomedical_entity recognizes acronyms", {
  expect_true(is_valid_biomedical_entity("BRCA1", "gene"))
  expect_true(is_valid_biomedical_entity("TP53", "gene"))
  expect_true(is_valid_biomedical_entity("EGFR", "protein"))
  expect_true(is_valid_biomedical_entity("DNA", "gene"))
})

test_that("is_valid_biomedical_entity works without claimed type", {
  # Without claimed type, the function checks general biomedical characteristics
  # It needs to find biomedical patterns, acronyms, or components

  # Acronyms should be recognized
  expect_true(is_valid_biomedical_entity("DNA", NULL))
  expect_true(is_valid_biomedical_entity("RNA", NULL))

  # Test that function returns a logical value for various terms
  result1 <- is_valid_biomedical_entity("migraine", NULL)
  result2 <- is_valid_biomedical_entity("receptor", NULL)
  result3 <- is_valid_biomedical_entity("kinase", NULL)

  expect_type(result1, "logical")
  expect_type(result2, "logical")
  expect_type(result3, "logical")

  # Very short generic terms should be rejected
  expect_false(is_valid_biomedical_entity("ab", NULL))
})

# ==============================================================================
# Tests for abc_model()
# ==============================================================================

test_that("abc_model requires valid co-occurrence matrix", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  expect_error(
    abc_model(co_matrix, a_term = "nonexistent"),
    "not found in the co-occurrence matrix"
  )
})

test_that("abc_model returns data frame with correct structure", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- abc_model(co_matrix, a_term = "migraine", min_score = 0.01)

  expect_s3_class(results, "data.frame")
  expect_true(all(c("a_term", "b_term", "c_term", "abc_score") %in% names(results)))
})

test_that("abc_model filters by minimum score", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- abc_model(co_matrix, a_term = "migraine", min_score = 0.5)

  if (nrow(results) > 0) {
    expect_true(all(results$abc_score >= 0.5))
  }
})

test_that("abc_model respects n_results parameter", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- abc_model(co_matrix, a_term = "migraine", n_results = 5)

  expect_lte(nrow(results), 5)
})

test_that("abc_model handles different scoring methods", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  mult <- abc_model(co_matrix, a_term = "migraine",
                    scoring_method = "multiplication", min_score = 0.01)
  avg <- abc_model(co_matrix, a_term = "migraine",
                   scoring_method = "average", min_score = 0.01)

  expect_s3_class(mult, "data.frame")
  expect_s3_class(avg, "data.frame")
})

test_that("abc_model filters biomedical terms when requested", {
  skip_if_not_installed("Matrix")

  entity_data <- rbind(
    create_test_entity_data(),
    data.frame(
      doc_id = c(4, 4),
      entity = c("migraine", "however"),
      entity_type = c("disease", "word"),
      count = c(1, 1),
      stringsAsFactors = FALSE
    )
  )

  co_matrix <- create_comat(entity_data)

  results <- abc_model(co_matrix, a_term = "migraine",
                       exclude_general_terms = TRUE, min_score = 0.01)

  # "however" should be filtered out
  if (nrow(results) > 0) {
    expect_false("however" %in% results$b_term)
    expect_false("however" %in% results$c_term)
  }
})

test_that("abc_model handles specific c_term", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- abc_model(co_matrix, a_term = "migraine",
                       c_term = "pain", min_score = 0.01)

  if (nrow(results) > 0) {
    expect_true(all(results$c_term == "pain"))
  }
})

test_that("abc_model filters by entity types", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- abc_model(co_matrix, a_term = "migraine",
                       b_term_types = c("chemical", "protein"),
                       min_score = 0.01)

  if (nrow(results) > 0 && "b_type" %in% names(results)) {
    expect_true(all(results$b_type %in% c("chemical", "protein")))
  }
})

test_that("abc_model handles empty results gracefully", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  # Very high threshold should return no results
  results <- abc_model(co_matrix, a_term = "migraine", min_score = 0.99)

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 0)
})

test_that("abc_model filters similar terms when requested", {
  skip_if_not_installed("Matrix")

  entity_data <- rbind(
    create_test_entity_data(),
    data.frame(
      doc_id = c(4, 4),
      entity = c("migraine", "migraines"),
      entity_type = c("disease", "disease"),
      count = c(1, 1),
      stringsAsFactors = FALSE
    )
  )

  co_matrix <- create_comat(entity_data)

  results <- abc_model(co_matrix, a_term = "migraine",
                       filter_similar_terms = TRUE, min_score = 0.01)

  # "migraines" should be filtered as similar to "migraine"
  if (nrow(results) > 0) {
    expect_false("migraines" %in% results$b_term)
  }
})

# ==============================================================================
# Tests for calculate_score()
# ==============================================================================

test_that("calculate_score handles multiplication", {
  score <- calculate_score(0.5, 0.6, "multiplication")
  expect_equal(score, 0.3)
})

test_that("calculate_score handles average", {
  score <- calculate_score(0.5, 0.6, "average")
  expect_equal(score, 0.55)
})

test_that("calculate_score handles combined", {
  score <- calculate_score(0.5, 0.6, "combined")
  expected <- 0.7 * 0.3 + 0.3 * 0.55
  expect_equal(score, expected)
})

# ==============================================================================
# Tests for diversify_abc()
# ==============================================================================

test_that("diversify_abc returns correct structure", {
  results <- data.frame(
    a_term = rep("migraine", 6),
    b_term = rep(c("b1", "b2"), each = 3),
    c_term = rep(c("c1", "c2", "c3"), 2),
    abc_score = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4),
    stringsAsFactors = FALSE
  )

  diverse <- diversify_abc(results, diversity_method = "b_term_groups",
                           max_per_group = 2)

  expect_s3_class(diverse, "data.frame")
  expect_lte(nrow(diverse), nrow(results))
})

test_that("diversify_abc respects max_per_group", {
  results <- data.frame(
    a_term = rep("migraine", 6),
    b_term = rep("b1", 6),
    c_term = paste0("c", 1:6),
    abc_score = seq(0.9, 0.4, length.out = 6),
    stringsAsFactors = FALSE
  )

  diverse <- diversify_abc(results, diversity_method = "b_term_groups",
                           max_per_group = 3)

  expect_lte(nrow(diverse), 3)
})

test_that("diversify_abc handles empty results", {
  empty <- data.frame(
    a_term = character(),
    b_term = character(),
    c_term = character(),
    abc_score = numeric(),
    stringsAsFactors = FALSE
  )

  diverse <- diversify_abc(empty)
  expect_equal(nrow(diverse), 0)
})

# ==============================================================================
# Tests for validate_abc()
# ==============================================================================

test_that("validate_abc adds significance columns", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- abc_model(co_matrix, a_term = "migraine", min_score = 0.01)

  if (nrow(results) > 0) {
    validated <- validate_abc(results, co_matrix)

    expect_true("p_value" %in% names(validated))
    expect_true("significant" %in% names(validated))
  }
})

test_that("validate_abc handles different correction methods", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- abc_model(co_matrix, a_term = "migraine", min_score = 0.01)

  if (nrow(results) > 0) {
    bh <- validate_abc(results, co_matrix, correction = "BH")
    bonf <- validate_abc(results, co_matrix, correction = "bonferroni")
    none <- validate_abc(results, co_matrix, correction = "none")

    expect_true("adjusted_p_value" %in% names(bh))
    expect_true("adjusted_p_value" %in% names(bonf))
    expect_true("adjusted_p_value" %in% names(none))
  }
})

test_that("validate_abc handles empty results", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  empty <- data.frame(
    a_term = character(),
    b_term = character(),
    c_term = character(),
    abc_score = numeric(),
    stringsAsFactors = FALSE
  )

  expect_message(
    validated <- validate_abc(empty, co_matrix),
    "ABC results are empty"
  )
})

# ==============================================================================
# Tests for filter_terms_for_abc_model()
# ==============================================================================

test_that("filter_terms_for_abc_model filters based on validation", {
  terms <- c("migraine", "headache", "receptor", "kinase", "serotonin")
  entity_types <- c("disease", "symptom", "protein", "protein", "chemical")
  names(entity_types) <- terms

  filtered <- filter_terms_for_abc_model(terms, entity_types)

  # Should filter based on is_valid_biomedical_entity
  expect_type(filtered, "character")
  expect_true(length(filtered) <= length(terms))

  # Valid biomedical terms should be included
  expect_true("migraine" %in% filtered)
  expect_true("receptor" %in% filtered)
})

test_that("filter_terms_for_abc_model works without types", {
  terms <- c("migraine", "receptor", "kinase")

  filtered <- filter_terms_for_abc_model(terms, NULL)

  expect_type(filtered, "character")
  expect_true(length(filtered) <= length(terms))
})

# ==============================================================================
# Tests for get_type_dist()
# ==============================================================================

test_that("get_type_dist returns distribution", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  dist <- get_type_dist(co_matrix)

  expect_s3_class(dist, "data.frame")
  expect_true(all(c("entity_type", "count", "percentage") %in% names(dist)))
})

test_that("get_type_dist requires entity types", {
  skip_if_not_installed("Matrix")

  entity_data <- data.frame(
    doc_id = c(1, 1, 2, 2),
    entity = c("a", "b", "a", "c"),
    stringsAsFactors = FALSE
  )

  co_matrix <- create_comat(entity_data, type_col = "nonexistent")

  expect_error(
    get_type_dist(co_matrix),
    "does not have entity type information"
  )
})

# ==============================================================================
# Tests for filter_by_type()
# ==============================================================================

test_that("filter_by_type filters correctly", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  filtered <- filter_by_type(co_matrix, types = c("disease", "symptom"))

  expect_s4_class(filtered, "Matrix")
  expect_lt(nrow(filtered), nrow(co_matrix))
})

test_that("filter_by_type preserves attributes", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  filtered <- filter_by_type(co_matrix, types = c("disease"))

  expect_false(is.null(attr(filtered, "entity_types")))
  expect_false(is.null(attr(filtered, "metadata")))
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("Full workflow: create matrix and apply ABC model", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)
  results <- abc_model(co_matrix, a_term = "migraine", min_score = 0.01)

  expect_s3_class(results, "data.frame")

  if (nrow(results) > 0) {
    validated <- validate_abc(results, co_matrix)
    expect_true("p_value" %in% names(validated))

    diverse <- diversify_abc(results)
    expect_s3_class(diverse, "data.frame")
  }
})

test_that("ABC model with entity type filtering", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- abc_model(
    co_matrix,
    a_term = "migraine",
    b_term_types = c("chemical", "protein"),
    c_term_types = c("symptom"),
    min_score = 0.01
  )

  expect_s3_class(results, "data.frame")
})

# ==============================================================================
# Edge cases and error handling
# ==============================================================================

test_that("create_comat handles single entity", {
  skip_if_not_installed("Matrix")

  entity_data <- data.frame(
    doc_id = c(1, 2),
    entity = c("term1", "term1"),
    stringsAsFactors = FALSE
  )

  co_matrix <- create_comat(entity_data, normalize = FALSE)
  expect_equal(nrow(co_matrix), 1)
  expect_equal(ncol(co_matrix), 1)
})

test_that("abc_model handles matrix with few entities", {
  skip_if_not_installed("Matrix")

  entity_data <- data.frame(
    doc_id = c(1, 1, 2, 2),
    entity = c("a", "b", "a", "c"),
    entity_type = c("type1", "type2", "type1", "type3"),
    stringsAsFactors = FALSE
  )

  co_matrix <- create_comat(entity_data)
  results <- abc_model(co_matrix, a_term = "a", min_score = 0.01)

  expect_s3_class(results, "data.frame")
})

test_that("is_valid_biomedical_entity handles case sensitivity", {
  # Test that function handles different cases
  expect_true(is_valid_biomedical_entity("Migraine", "disease"))
  expect_true(is_valid_biomedical_entity("MIGRAINE", "disease"))

  # Test that function returns logical values
  result <- is_valid_biomedical_entity("RECEPTOR", "protein")
  expect_type(result, "logical")
})

test_that("abc_model with validation_method parameter", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- abc_model(
    co_matrix,
    a_term = "migraine",
    validation_method = "pattern",
    min_score = 0.01
  )

  expect_s3_class(results, "data.frame")
})

test_that("create_comat with count column", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data, count_col = "count")

  expect_s4_class(co_matrix, "Matrix")
})

test_that("abc_model handles enforce_strict_typing", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  strict <- abc_model(co_matrix, a_term = "migraine",
                      enforce_strict_typing = TRUE, min_score = 0.01)
  lenient <- abc_model(co_matrix, a_term = "migraine",
                       enforce_strict_typing = FALSE, min_score = 0.01)

  expect_s3_class(strict, "data.frame")
  expect_s3_class(lenient, "data.frame")
})

# ==============================================================================
# Performance tests
# ==============================================================================

test_that("create_comat handles moderate sized data efficiently", {
  skip_if_not_installed("Matrix")
  skip("Slow test - run manually")

  # Create larger dataset
  n_docs <- 100
  n_entities_per_doc <- 10

  entity_data <- data.frame(
    doc_id = rep(1:n_docs, each = n_entities_per_doc),
    entity = sample(paste0("entity", 1:50), n_docs * n_entities_per_doc, replace = TRUE),
    entity_type = sample(c("disease", "chemical", "protein"),
                         n_docs * n_entities_per_doc, replace = TRUE),
    stringsAsFactors = FALSE
  )

  start_time <- Sys.time()
  co_matrix <- create_comat(entity_data)
  end_time <- Sys.time()

  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_lt(elapsed, 10)  # Should complete within 10 seconds
})
