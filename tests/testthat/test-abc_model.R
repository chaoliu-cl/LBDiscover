library(testthat)
library(mockery)

# Create mock data for abc_model testing
create_mock_comat <- function() {
  # Create a small co-occurrence matrix for testing
  comat <- matrix(c(
    1.0, 0.5, 0.3, 0.1, 0.0,
    0.5, 1.0, 0.6, 0.2, 0.0,
    0.3, 0.6, 1.0, 0.7, 0.2,
    0.1, 0.2, 0.7, 1.0, 0.4,
    0.0, 0.0, 0.2, 0.4, 1.0
  ), nrow = 5, ncol = 5, byrow = TRUE)

  rownames(comat) <- c("term_A", "term_B", "term_C", "term_D", "term_E")
  colnames(comat) <- c("term_A", "term_B", "term_C", "term_D", "term_E")

  # Add entity types as an attribute
  entity_types <- c(
    "term_A" = "disease",
    "term_B" = "drug",
    "term_C" = "gene",
    "term_D" = "protein",
    "term_E" = "pathway"
  )
  attr(comat, "entity_types") <- entity_types

  return(comat)
}

test_that("abc_model returns correct structure and data", {
  # Create mock co-occurrence matrix
  mock_comat <- create_mock_comat()

  # Run ABC model with mock data
  results <- abc_model(
    co_matrix = mock_comat,
    a_term = "term_A",
    min_score = 0.1,
    n_results = 10
  )

  # Test structure of results
  expect_true(is.data.frame(results))
  expect_true(all(c("a_term", "b_term", "c_term", "a_b_score", "b_c_score", "abc_score") %in% colnames(results)))

  # Verify that all results contain term_A as the A term
  expect_true(all(results$a_term == "term_A"))

  # Verify that scores are within expected range [0, 1]
  expect_true(all(results$a_b_score >= 0 & results$a_b_score <= 1))
  expect_true(all(results$b_c_score >= 0 & results$b_c_score <= 1))
  expect_true(all(results$abc_score >= 0 & results$abc_score <= 1))

  # Test that filtering by min_score works
  expect_true(all(results$a_b_score >= 0.1))
  expect_true(all(results$b_c_score >= 0.1))
})

test_that("abc_model handles entity type constraints correctly", {
  # Create mock co-occurrence matrix
  mock_comat <- create_mock_comat()

  # Run ABC model with B term type constraint
  results_drug <- abc_model(
    co_matrix = mock_comat,
    a_term = "term_A",
    min_score = 0.1,
    b_term_types = "drug"
  )

  # Check that all B terms have type "drug"
  if (nrow(results_drug) > 0) {
    expect_true(all(results_drug$b_type == "drug"))
  }

  # Run model with C term type constraint
  results_pathway <- abc_model(
    co_matrix = mock_comat,
    a_term = "term_A",
    min_score = 0.1,
    c_term_types = "pathway"
  )

  # Check that all C terms have type "pathway"
  if (nrow(results_pathway) > 0) {
    expect_true(all(results_pathway$c_type == "pathway"))
  }
})

test_that("abc_model applies scoring methods correctly", {
  # Create mock co-occurrence matrix
  mock_comat <- create_mock_comat()

  # Test multiplication scoring method
  results_mult <- abc_model(
    co_matrix = mock_comat,
    a_term = "term_A",
    min_score = 0.1,
    scoring_method = "multiplication"
  )

  # Test that ABC score equals a_b_score * b_c_score
  if (nrow(results_mult) > 0) {
    for (i in 1:nrow(results_mult)) {
      expect_equal(
        results_mult$abc_score[i],
        results_mult$a_b_score[i] * results_mult$b_c_score[i]
      )
    }
  }

  # Test average scoring method
  results_avg <- abc_model(
    co_matrix = mock_comat,
    a_term = "term_A",
    min_score = 0.1,
    scoring_method = "average"
  )

  # Now we'll need to check if the results have the $abc_score_avg column
  # Since the original function might calculate this differently, adapt as needed
  if (nrow(results_avg) > 0 && "abc_score_avg" %in% colnames(results_avg)) {
    for (i in 1:nrow(results_avg)) {
      expect_equal(
        results_avg$abc_score_avg[i],
        (results_avg$a_b_score[i] + results_avg$b_c_score[i]) / 2
      )
    }
  }
})

test_that("abc_model handles error cases gracefully", {
  # Create mock co-occurrence matrix
  mock_comat <- create_mock_comat()

  # Test with non-existent A term
  expect_error(
    abc_model(mock_comat, a_term = "nonexistent_term"),
    "A-term 'nonexistent_term' not found in the co-occurrence matrix"
  )

  # Test with non-existent C term
  expect_error(
    abc_model(mock_comat, a_term = "term_A", c_term = "nonexistent_term"),
    "C-term 'nonexistent_term' not found in the co-occurrence matrix"
  )

  # Test with min_score that filters out all results
  high_threshold_results <- abc_model(
    mock_comat,
    a_term = "term_A",
    min_score = 0.9
  )
  expect_true(is.data.frame(high_threshold_results))
  expect_equal(nrow(high_threshold_results), 0)
})

test_that("is_valid_biomedical_entity correctly validates entities", {
  # Test positive cases
  expect_true(is_valid_biomedical_entity("migraine", "disease"))
  expect_true(is_valid_biomedical_entity("ibuprofen", "drug"))
  expect_true(is_valid_biomedical_entity("BRCA1", "gene"))
  expect_true(is_valid_biomedical_entity("receptor", "protein"))

  # Test negative cases
  expect_false(is_valid_biomedical_entity("europe", "disease"))
  expect_false(is_valid_biomedical_entity("the", "gene"))
  expect_false(is_valid_biomedical_entity("optimization", "chemical"))
  expect_false(is_valid_biomedical_entity("analysis", "drug"))

  # Test case sensitivity
  expect_true(is_valid_biomedical_entity("Migraine", "disease"))
  expect_true(is_valid_biomedical_entity("IBUPROFEN", "drug"))
})

test_that("calculate_score applies different scoring methods correctly", {
  # Test multiplication method
  expect_equal(calculate_score(0.5, 0.6, "multiplication"), 0.3)

  # Test average method
  expect_equal(calculate_score(0.5, 0.6, "average"), 0.55)

  # Test combined method (0.7 * multiplication + 0.3 * average)
  expected <- 0.7 * (0.5 * 0.6) + 0.3 * ((0.5 + 0.6) / 2)
  expect_equal(calculate_score(0.5, 0.6, "combined"), expected)

  # Test default method
  expect_equal(calculate_score(0.5, 0.6, "non_existent_method"), 0.5 * 0.6)
})

test_that("diversify_b_terms creates diverse results", {
  # Create a simple results data frame with repeated B terms
  test_results <- data.frame(
    a_term = rep("A", 9),
    b_term = rep(c("B1", "B2", "B3"), each = 3),
    c_term = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"),
    a_b_score = rep(0.5, 9),
    b_c_score = rep(0.6, 9),
    abc_score = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
    stringsAsFactors = FALSE
  )

  # Apply diversification
  diverse_results <- diversify_b_terms(test_results, max_per_group = 2)

  # Check that we have at most 2 results per B term
  expect_true(all(table(diverse_results$b_term) <= 2))

  # Check that we keep the highest scoring results for each B term
  b1_results <- diverse_results[diverse_results$b_term == "B1", ]
  expect_true(all(b1_results$c_term %in% c("C1", "C2")))
})

test_that("validate_abc applies statistical validation correctly", {
  # Create a mock ABC results data frame
  test_results <- data.frame(
    a_term = rep("term_A", 3),
    b_term = c("term_B", "term_B", "term_C"),
    c_term = c("term_D", "term_E", "term_E"),
    a_b_score = c(0.5, 0.5, 0.3),
    b_c_score = c(0.6, 0.4, 0.6),
    abc_score = c(0.3, 0.2, 0.18),
    stringsAsFactors = FALSE
  )

  # Create a mock co-occurrence matrix
  mock_comat <- create_mock_comat()

  # Apply validation
  validated_results <- validate_abc(
    test_results,
    mock_comat,
    alpha = 0.05,
    correction = "none"
  )

  # Check that validation adds expected columns
  expect_true(all(c("p_value", "adjusted_p_value", "significant") %in% colnames(validated_results)))

  # Check that p-values are between 0 and 1
  expect_true(all(validated_results$p_value >= 0 & validated_results$p_value <= 1))
  expect_true(all(validated_results$adjusted_p_value >= 0 & validated_results$adjusted_p_value <= 1))

  # Check that significant column is logical
  expect_true(is.logical(validated_results$significant))
})
