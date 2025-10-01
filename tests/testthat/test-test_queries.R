# Test file for queries.R functions
# This file tests query_umls(), query_mesh(), and enhance_abc_kb()

library(testthat)

# Skip tests if required packages are not available
skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste("Package", pkg, "not available"))
  }
}

# Skip if no internet connection
skip_if_offline <- function() {
  tryCatch({
    con <- url("https://www.google.com", open = "rb")
    close(con)
  }, error = function(e) {
    skip("No internet connection available")
  })
}

# ==============================================================================
# Tests for query_umls()
# ==============================================================================

test_that("query_umls requires an API key", {
  expect_error(
    query_umls("migraine", api_key = NULL),
    "UMLS API key is required"
  )
})

test_that("query_umls returns correct structure with valid inputs", {
  skip_if_offline()
  skip("Requires valid UMLS API key")

  # Note: Replace with actual API key for testing
  api_key <- Sys.getenv("UMLS_API_KEY")
  if (api_key == "") {
    skip("No UMLS API key found in environment")
  }

  result <- query_umls("migraine", api_key = api_key)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("cui", "term", "semantic_type", "source", "definition"))
  expect_equal(nrow(result), 1)
  expect_equal(result$source, "UMLS")
})

test_that("query_umls handles non-existent terms gracefully", {
  skip_if_offline()
  skip("Requires valid UMLS API key")

  api_key <- Sys.getenv("UMLS_API_KEY")
  if (api_key == "") {
    skip("No UMLS API key found in environment")
  }

  result <- query_umls("xyznonexistentterm123", api_key = api_key)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$cui) || result$semantic_type == "Unknown")
})

test_that("query_umls accepts version parameter", {
  skip_if_offline()
  skip("Requires valid UMLS API key")

  api_key <- Sys.getenv("UMLS_API_KEY")
  if (api_key == "") {
    skip("No UMLS API key found in environment")
  }

  result <- query_umls("headache", api_key = api_key, version = "current")

  expect_s3_class(result, "data.frame")
  expect_equal(result$source, "UMLS")
})

# ==============================================================================
# Tests for query_mesh()
# ==============================================================================

test_that("query_mesh returns correct structure", {
  skip_if_offline()
  skip_if_not_installed("rentrez")
  skip_if_not_installed("xml2")

  result <- query_mesh("migraine")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("mesh_id", "term", "tree_number", "scope_note"))
  expect_equal(nrow(result), 1)
})

test_that("query_mesh works without API key", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  result <- query_mesh("headache", api_key = NULL)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("query_mesh works with API key", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  api_key <- Sys.getenv("NCBI_API_KEY")
  result <- query_mesh("pain", api_key = if(api_key != "") api_key else NULL)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("query_mesh handles non-existent terms", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  result <- query_mesh("xyznonexistentterm123")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$mesh_id))
  expect_true(grepl("No MeSH term found", result$scope_note))
})

test_that("query_mesh extracts MeSH ID correctly", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  result <- query_mesh("diabetes mellitus")

  expect_s3_class(result, "data.frame")
  if (!is.na(result$mesh_id)) {
    expect_match(result$mesh_id, "^[A-Z][0-9]+$")
  }
})

test_that("query_mesh extracts tree numbers correctly", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  result <- query_mesh("aspirin")

  expect_s3_class(result, "data.frame")
  if (!is.na(result$tree_number) && result$tree_number != "") {
    expect_match(result$tree_number, "[A-Z][0-9\\.,\\s]+")
  }
})

test_that("query_mesh handles errors gracefully", {
  skip_if_not_installed("rentrez")

  # Test with empty string
  result <- query_mesh("")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

# ==============================================================================
# Tests for enhance_abc_kb()
# ==============================================================================

test_that("enhance_abc_kb requires valid knowledge base", {
  abc_results <- data.frame(
    a_term = "migraine",
    b_terms = "serotonin, receptor",
    c_term = "sumatriptan",
    abc_score = 0.8,
    stringsAsFactors = FALSE
  )

  expect_error(
    enhance_abc_kb(abc_results, knowledge_base = "invalid"),
    "'arg' should be one of"
  )
})

test_that("enhance_abc_kb handles empty results", {
  empty_results <- data.frame(
    a_term = character(),
    b_terms = character(),
    c_term = character(),
    abc_score = numeric(),
    stringsAsFactors = FALSE
  )

  result <- enhance_abc_kb(empty_results, knowledge_base = "mesh")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("enhance_abc_kb with mesh adds correct columns", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  abc_results <- data.frame(
    a_term = "migraine",
    b_terms = "serotonin",
    c_term = "headache",
    abc_score = 0.8,
    stringsAsFactors = FALSE
  )

  result <- enhance_abc_kb(abc_results, knowledge_base = "mesh")

  expect_s3_class(result, "data.frame")
  expect_true("a_mesh_id" %in% names(result))
  expect_true("a_tree_number" %in% names(result))
  expect_true("c_mesh_id" %in% names(result))
  expect_true("c_tree_number" %in% names(result))
  expect_equal(nrow(result), nrow(abc_results))
})

test_that("enhance_abc_kb with umls adds correct columns", {
  skip_if_offline()
  skip("Requires valid UMLS API key")

  api_key <- Sys.getenv("UMLS_API_KEY")
  if (api_key == "") {
    skip("No UMLS API key found in environment")
  }

  abc_results <- data.frame(
    a_term = "migraine",
    b_terms = "serotonin",
    c_term = "headache",
    abc_score = 0.8,
    stringsAsFactors = FALSE
  )

  result <- enhance_abc_kb(abc_results, knowledge_base = "umls", api_key = api_key)

  expect_s3_class(result, "data.frame")
  expect_true("a_cui" %in% names(result))
  expect_true("a_semantic_type" %in% names(result))
  expect_true("c_cui" %in% names(result))
  expect_true("c_semantic_type" %in% names(result))
  expect_equal(nrow(result), nrow(abc_results))
})

test_that("enhance_abc_kb processes multiple unique terms", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  abc_results <- data.frame(
    a_term = c("migraine", "migraine", "diabetes"),
    b_terms = c("serotonin", "dopamine", "insulin"),
    c_term = c("headache", "nausea", "glucose"),
    abc_score = c(0.8, 0.7, 0.9),
    stringsAsFactors = FALSE
  )

  result <- enhance_abc_kb(abc_results, knowledge_base = "mesh")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(abc_results))
  expect_equal(ncol(result), ncol(abc_results) + 4)
})

test_that("enhance_abc_kb handles b_terms with commas", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  abc_results <- data.frame(
    a_term = "migraine",
    b_terms = "serotonin, dopamine, receptor",
    c_term = "headache",
    abc_score = 0.8,
    stringsAsFactors = FALSE
  )

  result <- enhance_abc_kb(abc_results, knowledge_base = "mesh")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("enhance_abc_kb preserves original columns", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  abc_results <- data.frame(
    a_term = "migraine",
    b_terms = "serotonin",
    c_term = "headache",
    abc_score = 0.8,
    extra_column = "test",
    stringsAsFactors = FALSE
  )

  result <- enhance_abc_kb(abc_results, knowledge_base = "mesh")

  expect_true("extra_column" %in% names(result))
  expect_equal(result$extra_column, "test")
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("query_mesh and enhance_abc_kb work together", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  # First query a single term
  mesh_result <- query_mesh("migraine")
  expect_s3_class(mesh_result, "data.frame")

  # Then enhance ABC results
  abc_results <- data.frame(
    a_term = "migraine",
    b_terms = "serotonin",
    c_term = "headache",
    abc_score = 0.8,
    stringsAsFactors = FALSE
  )

  enhanced <- enhance_abc_kb(abc_results, knowledge_base = "mesh")
  expect_s3_class(enhanced, "data.frame")
  expect_true("a_mesh_id" %in% names(enhanced))
})

test_that("enhance_abc_kb handles NA values in results", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  abc_results <- data.frame(
    a_term = c("migraine", NA),
    b_terms = c("serotonin", "dopamine"),
    c_term = c("headache", "nausea"),
    abc_score = c(0.8, 0.7),
    stringsAsFactors = FALSE
  )

  # Should handle NA gracefully
  result <- enhance_abc_kb(abc_results, knowledge_base = "mesh")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(abc_results))
})

# ==============================================================================
# Edge cases and error handling
# ==============================================================================

test_that("query_mesh handles special characters in terms", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  result <- query_mesh("α-synuclein")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("enhance_abc_kb handles large result sets", {
  skip_if_offline()
  skip_if_not_installed("rentrez")
  skip("Slow test - only run manually")

  # Create large ABC results
  n <- 50
  abc_results <- data.frame(
    a_term = rep("migraine", n),
    b_terms = paste("term", 1:n),
    c_term = paste("result", 1:n),
    abc_score = runif(n),
    stringsAsFactors = FALSE
  )

  result <- enhance_abc_kb(abc_results, knowledge_base = "mesh")
  expect_equal(nrow(result), n)
})

test_that("query_mesh and query_umls return consistent structure", {
  skip_if_offline()
  skip_if_not_installed("rentrez")
  skip("Requires valid UMLS API key")

  api_key <- Sys.getenv("UMLS_API_KEY")
  if (api_key == "") {
    skip("No UMLS API key found in environment")
  }

  mesh_result <- query_mesh("migraine")
  umls_result <- query_umls("migraine", api_key = api_key)

  # Both should return data frames with 1 row
  expect_equal(nrow(mesh_result), 1)
  expect_equal(nrow(umls_result), 1)

  # Both should have term information
  expect_false(is.na(mesh_result$term))
  expect_false(is.na(umls_result$term))
})

# ==============================================================================
# Performance and timeout tests
# ==============================================================================

test_that("query_mesh completes in reasonable time", {
  skip_if_offline()
  skip_if_not_installed("rentrez")

  start_time <- Sys.time()
  result <- query_mesh("aspirin")
  end_time <- Sys.time()

  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_lt(elapsed, 10)  # Should complete within 10 seconds
})
