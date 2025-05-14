# Tests for heatmap visualization functions

# Create mock ABC results for testing
create_mock_abc_results <- function(n_results = 50) {
  set.seed(123)  # For reproducibility

  # Generate A, B, and C terms
  a_terms <- paste0("A_Term_", 1:2)
  b_terms <- paste0("B_Term_", 1:10)
  c_terms <- paste0("C_Term_", 1:8)

  # Generate entity types
  entity_types <- c("disease", "drug", "gene", "protein", "pathway")

  # Create results data frame
  results <- data.frame(
    a_term = sample(a_terms, n_results, replace = TRUE),
    b_term = sample(b_terms, n_results, replace = TRUE),
    c_term = sample(c_terms, n_results, replace = TRUE),
    a_b_score = runif(n_results, 0.1, 0.9),
    b_c_score = runif(n_results, 0.1, 0.9),
    abc_score = runif(n_results, 0.01, 0.5),
    stringsAsFactors = FALSE
  )

  # Add entity types if requested
  results$a_type <- sample(entity_types, n_results, replace = TRUE)
  results$b_type <- sample(entity_types, n_results, replace = TRUE)
  results$c_type <- sample(entity_types, n_results, replace = TRUE)

  # Add significance values
  results$p_value <- runif(n_results, 0, 0.1)
  results$significant <- results$p_value < 0.05

  # Ensure there are no duplicate A-B-C combinations
  results <- unique(results[, c("a_term", "b_term", "c_term", "a_b_score", "b_c_score", "abc_score",
                                "a_type", "b_type", "c_type", "p_value", "significant")])

  return(results)
}

# Test vis_heatmap function
test_that("vis_heatmap creates a heatmap plot", {
  # Skip if not in interactive session
  skip_if_not(interactive(), "Skipping visualization tests in non-interactive mode")

  # Create mock results
  mock_results <- create_mock_abc_results(30)

  # Test basic heatmap
  expect_silent(vis_heatmap(mock_results, top_n = 10))

  # Test with different parameters
  expect_silent(vis_heatmap(mock_results, top_n = 15, min_score = 0.05,
                            show_significance = TRUE, title = "Custom Title"))

  # Test with entity type display
  expect_silent(vis_heatmap(mock_results, show_entity_types = TRUE))

  # Test with different color palette
  expect_silent(vis_heatmap(mock_results, color_palette = "reds"))
})

# Test vis_abc_heatmap function
test_that("vis_abc_heatmap creates a basic heatmap plot", {
  # Skip if not in interactive session
  skip_if_not(interactive(), "Skipping visualization tests in non-interactive mode")

  # Create mock results
  mock_results <- create_mock_abc_results(30)

  # Test basic heatmap
  expect_silent(vis_abc_heatmap(mock_results, top_n = 10))

  # Test with different parameters
  expect_silent(vis_abc_heatmap(mock_results, top_n = 15, min_score = 0.05,
                                show_labels = TRUE, title = "Custom Heatmap"))

  # Test with single A term
  single_a_results <- mock_results[mock_results$a_term == unique(mock_results$a_term)[1], ]
  expect_silent(vis_abc_heatmap(single_a_results))
})

# Test error handling in heatmap functions
test_that("heatmap functions handle errors properly", {
  # Create empty results
  empty_results <- data.frame(
    a_term = character(0),
    b_term = character(0),
    c_term = character(0),
    a_b_score = numeric(0),
    b_c_score = numeric(0),
    abc_score = numeric(0)
  )

  # Test with empty results
  expect_error(vis_heatmap(empty_results), "ABC results are empty")
  expect_error(vis_abc_heatmap(empty_results), "ABC results are empty")

  # Create results with scores below threshold
  low_score_results <- create_mock_abc_results(10)
  low_score_results$abc_score <- 0.001  # All scores below default threshold

  # Test with all scores below threshold
  expect_error(vis_heatmap(low_score_results, min_score = 0.1), "No results remain after filtering")
  expect_error(vis_abc_heatmap(low_score_results, min_score = 0.1), "No results remain after filtering")
})

# Test with many terms (should handle gracefully)
test_that("heatmap functions handle many terms gracefully", {
  # Skip if not in interactive session
  skip_if_not(interactive(), "Skipping visualization tests in non-interactive mode")

  # Create results with many terms
  set.seed(456)
  many_terms_results <- data.frame(
    a_term = rep("A_Term_1", 100),
    b_term = paste0("B_Term_", 1:30),
    c_term = paste0("C_Term_", 1:25),
    a_b_score = runif(100, 0.1, 0.9),
    b_c_score = runif(100, 0.1, 0.9),
    abc_score = runif(100, 0.1, 0.5),
    stringsAsFactors = FALSE
  )

  # Test that it handles many terms without error
  expect_message(vis_heatmap(many_terms_results), "Limiting visualization")
  expect_message(vis_abc_heatmap(many_terms_results), "Limiting visualization")
})
