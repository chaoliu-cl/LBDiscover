library(testthat)

# Define the mock function locally if not available globally
if (!exists("create_mock_abc_results")) {
  create_mock_abc_results <- function(n_results = 50) {
    set.seed(123)

    a_terms <- paste0("A_Term_", 1:2)
    b_terms <- paste0("B_Term_", 1:10)
    c_terms <- paste0("C_Term_", 1:8)
    entity_types <- c("disease", "drug", "gene", "protein", "pathway")

    results <- data.frame(
      a_term = sample(a_terms, n_results, replace = TRUE),
      b_term = sample(b_terms, n_results, replace = TRUE),
      c_term = sample(c_terms, n_results, replace = TRUE),
      a_b_score = runif(n_results, 0.1, 0.9),
      b_c_score = runif(n_results, 0.1, 0.9),
      abc_score = runif(n_results, 0.01, 0.5),
      stringsAsFactors = FALSE
    )

    results$a_type <- sample(entity_types, n_results, replace = TRUE)
    results$b_type <- sample(entity_types, n_results, replace = TRUE)
    results$c_type <- sample(entity_types, n_results, replace = TRUE)

    return(unique(results))
  }
}

test_that("vis_abc_heatmap handles empty results", {
  empty_results <- data.frame(
    a_term = character(),
    b_term = character(),
    c_term = character(),
    abc_score = numeric(),
    stringsAsFactors = FALSE
  )

  expect_error(vis_abc_heatmap(empty_results), "ABC results are empty")
})

test_that("create_report handles missing data gracefully", {
  # Test with empty results
  empty_results <- list(
    abc = data.frame(),
    anc = data.frame()
  )

  temp_file <- tempfile(fileext = ".html")
  result <- create_report(empty_results, output_file = temp_file)

  expect_true(file.exists(temp_file))
  expect_equal(result, temp_file)

  unlink(temp_file)
})

test_that("export_network handles various input sizes", {
  # Create small mock results that should work
  small_results <- create_mock_abc_results(10)

  temp_file <- tempfile(fileext = ".html")

  expect_no_error(export_network(small_results, output_file = temp_file,
                                 top_n = 10, open = FALSE))

  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)

  unlink(temp_file)
})
