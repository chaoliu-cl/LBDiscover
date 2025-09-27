library(testthat)

test_that("calculate_score handles edge cases", {
  # Test with zero scores
  expect_equal(calculate_score(0, 0, "multiplication"), 0)
  expect_equal(calculate_score(0, 1, "average"), 0.5)

  # Test with identical scores
  expect_equal(calculate_score(0.5, 0.5, "multiplication"), 0.25)
  expect_equal(calculate_score(0.5, 0.5, "average"), 0.5)

  # Test with boundary values
  expect_equal(calculate_score(1, 1, "multiplication"), 1)
  expect_equal(calculate_score(1, 1, "average"), 1)

  # Test combined method
  combined_score <- calculate_score(0.6, 0.8, "combined")
  expect_true(combined_score > 0 && combined_score < 1)
})

test_that("validation functions handle various inputs", {
  # Test with valid biomedical terms
  expect_true(is_valid_biomedical_entity("migraine", "disease"))
  expect_true(is_valid_biomedical_entity("receptor", "protein"))

  # Test with invalid terms
  expect_false(is_valid_biomedical_entity("optimization", "disease"))
  expect_false(is_valid_biomedical_entity("europe", "gene"))

  # Test with edge cases
  expect_false(is_valid_biomedical_entity("", "disease"))
  expect_false(is_valid_biomedical_entity("123", "protein"))
  expect_false(is_valid_biomedical_entity(NULL, "disease"))
})

test_that("diversify_b_terms works correctly", {
  # Create test data
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
})
