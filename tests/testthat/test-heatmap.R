library(testthat)

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
  skip_if_not_installed("graphics")
  skip_if_not_installed("grDevices")

  # Create mock results
  mock_results <- create_mock_abc_results(30)

  # Use null device for non-interactive testing
  pdf(NULL)
  on.exit(dev.off())

  # Test basic heatmap
  expect_no_error(vis_heatmap(mock_results, top_n = 10))

  # Test with different parameters
  expect_no_error(vis_heatmap(mock_results, top_n = 15, min_score = 0.05,
                              show_significance = TRUE, title = "Custom Title"))

  # Test with entity type display
  expect_no_error(vis_heatmap(mock_results, show_entity_types = TRUE))

  # Test with different color palette
  expect_no_error(vis_heatmap(mock_results, color_palette = "reds"))
})

# Test vis_abc_heatmap function
test_that("vis_abc_heatmap creates a basic heatmap plot", {
  skip_if_not_installed("graphics")
  skip_if_not_installed("grDevices")

  # Create mock results
  mock_results <- create_mock_abc_results(30)

  # Use null device for non-interactive testing
  pdf(NULL)
  on.exit(dev.off())

  # Test basic heatmap
  expect_no_error(vis_abc_heatmap(mock_results, top_n = 10))

  # Test with different parameters
  expect_no_error(vis_abc_heatmap(mock_results, top_n = 15, min_score = 0.05,
                                  show_labels = TRUE, title = "Custom Heatmap"))

  # Test with single A term
  single_a_results <- mock_results[mock_results$a_term == unique(mock_results$a_term)[1], ]
  expect_no_error(vis_abc_heatmap(single_a_results))
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
  skip_if_not_installed("graphics")

  # Create results with many terms - FIXED: proper vector lengths
  set.seed(456)
  many_terms_results <- data.frame(
    a_term = rep("A_Term_1", 100),
    b_term = rep(paste0("B_Term_", 1:30), length.out = 100),
    c_term = rep(paste0("C_Term_", 1:25), length.out = 100),
    a_b_score = runif(100, 0.1, 0.9),
    b_c_score = runif(100, 0.1, 0.9),
    abc_score = runif(100, 0.1, 0.5),
    stringsAsFactors = FALSE
  )

  # Use null device for non-interactive testing
  pdf(NULL)
  on.exit(dev.off())

  # Test that it handles many terms without error (may produce warnings about missing columns)
  suppressWarnings(expect_no_error(vis_heatmap(many_terms_results)))
  suppressWarnings(expect_no_error(vis_abc_heatmap(many_terms_results)))
})

# Test heatmap with different score ranges
test_that("heatmap functions handle different score ranges", {
  skip_if_not_installed("graphics")

  # Create results with very low scores
  low_score_results <- create_mock_abc_results(20)
  low_score_results$abc_score <- runif(nrow(low_score_results), 0.01, 0.1)

  pdf(NULL)
  on.exit(dev.off())

  expect_no_error(vis_heatmap(low_score_results, min_score = 0.01))

  # Create results with high scores
  high_score_results <- create_mock_abc_results(20)
  high_score_results$abc_score <- runif(nrow(high_score_results), 0.8, 0.95)

  expect_no_error(vis_heatmap(high_score_results, min_score = 0.5))
})

# Test heatmap filtering parameters
test_that("heatmap functions apply filtering correctly", {
  skip_if_not_installed("graphics")

  mock_results <- create_mock_abc_results(50)

  pdf(NULL)
  on.exit(dev.off())

  # Test top_n filtering
  expect_no_error(vis_heatmap(mock_results, top_n = 5))
  expect_no_error(vis_heatmap(mock_results, top_n = 20))

  # Test min_score filtering
  expect_no_error(vis_heatmap(mock_results, min_score = 0.1))
  expect_no_error(vis_heatmap(mock_results, min_score = 0.01))
})

# Test heatmap visualization parameters
test_that("heatmap functions accept visualization parameters", {
  skip_if_not_installed("graphics")

  mock_results <- create_mock_abc_results(20)

  pdf(NULL)
  on.exit(dev.off())

  # Test show_significance parameter
  expect_no_error(vis_heatmap(mock_results, show_significance = TRUE))
  expect_no_error(vis_heatmap(mock_results, show_significance = FALSE))

  # Test show_entity_types parameter
  expect_no_error(vis_heatmap(mock_results, show_entity_types = TRUE))
  expect_no_error(vis_heatmap(mock_results, show_entity_types = FALSE))

  # Test title parameter
  expect_no_error(vis_heatmap(mock_results, title = "Test Heatmap Title"))

  # Test color_palette parameter
  expect_no_error(vis_heatmap(mock_results, color_palette = "blues"))
  expect_no_error(vis_heatmap(mock_results, color_palette = "reds"))
  expect_no_error(vis_heatmap(mock_results, color_palette = "greens"))
})

# Test heatmap with missing optional columns
test_that("heatmap functions handle missing optional columns", {
  skip_if_not_installed("graphics")

  # Create results without significance columns
  basic_results <- data.frame(
    a_term = rep("A_Term_1", 10),
    b_term = paste0("B_Term_", 1:5),
    c_term = paste0("C_Term_", 1:5),
    a_b_score = runif(10, 0.1, 0.9),
    b_c_score = runif(10, 0.1, 0.9),
    abc_score = runif(10, 0.1, 0.5),
    stringsAsFactors = FALSE
  )

  pdf(NULL)
  on.exit(dev.off())

  # Should work without p_value and significant columns (may warn)
  suppressWarnings(expect_no_error(vis_heatmap(basic_results, show_significance = FALSE)))

  # Create results without entity type columns
  no_type_results <- basic_results

  # Should work without entity type columns (may warn)
  suppressWarnings(expect_no_error(vis_heatmap(no_type_results, show_entity_types = FALSE)))
})

# Test heatmap structure validation
test_that("heatmap functions validate input structure", {
  # Test with missing required columns
  incomplete_results <- data.frame(
    a_term = c("A1", "A2"),
    b_term = c("B1", "B2")
    # Missing c_term and scores
  )

  # These should error, but may also produce warnings - suppress warnings to focus on errors
  suppressWarnings(expect_error(vis_heatmap(incomplete_results)))
  suppressWarnings(expect_error(vis_abc_heatmap(incomplete_results)))
})

# Test heatmap with multiple A terms
test_that("vis_abc_heatmap handles multiple A terms", {
  skip_if_not_installed("graphics")

  # Create results with multiple A terms
  multi_a_results <- create_mock_abc_results(40)

  pdf(NULL)
  on.exit(dev.off())

  # Should handle multiple A terms
  expect_no_error(vis_abc_heatmap(multi_a_results, top_n = 20))
})

# Test heatmap label display
test_that("vis_abc_heatmap show_labels parameter works", {
  skip_if_not_installed("graphics")

  mock_results <- create_mock_abc_results(15)

  pdf(NULL)
  on.exit(dev.off())

  # Test with labels shown
  expect_no_error(vis_abc_heatmap(mock_results, show_labels = TRUE))

  # Test with labels hidden
  expect_no_error(vis_abc_heatmap(mock_results, show_labels = FALSE))
})
