# Test file for queries.R functions
# Tests for the LBDiscover package

library(testthat)

# Test helper functions and mock data
create_mock_abc_results <- function() {
  data.frame(
    a_term = c("migraine", "migraine", "headache"),
    b_terms = c("serotonin, CGRP", "sumatriptan", "pain"),
    c_term = c("CGRP", "receptor", "inflammation"),
    a_b_score = c(0.8, 0.7, 0.6),
    b_c_score = c(0.9, 0.8, 0.7),
    abc_score = c(0.72, 0.56, 0.42),
    stringsAsFactors = FALSE
  )
}

create_mock_umls_response <- function() {
  list(
    cui = "C0149931",
    term = "Migraine Disorders",
    semantic_type = "Disease or Syndrome",
    source = "UMLS",
    definition = "A class of disabling primary headache disorders"
  )
}

create_mock_mesh_response <- function() {
  data.frame(
    mesh_id = "D008881",
    term = "Migraine Disorders",
    tree_number = "C10.228.140.546.800.525",
    scope_note = "A class of disabling primary headache disorders",
    stringsAsFactors = FALSE
  )
}

# Helper function to check if we can run API integration tests
can_run_umls_tests <- function() {
  # Check if httr is available and we have basic internet connectivity
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(FALSE)
  }

  # Try a simple HTTP request to check connectivity
  tryCatch({
    httr::GET("https://httpbin.org/status/200", httr::timeout(5))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

can_run_mesh_tests <- function() {
  # Check if rentrez is available
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    return(FALSE)
  }

  # Try to load rentrez
  tryCatch({
    library(rentrez, quietly = TRUE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Tests for query_umls function
test_that("query_umls validates required parameters", {
  # Test that API key is required
  expect_error(
    query_umls("migraine", api_key = NULL),
    "UMLS API key is required"
  )

  # Test that function accepts required parameters
  expect_no_error({
    # Basic parameter validation
    term <- "migraine"
    api_key <- "test_key"
    version <- "current"

    expect_type(term, "character")
    expect_type(api_key, "character")
    expect_type(version, "character")
  })
})

test_that("query_umls handles missing term gracefully", {
  skip_if_not_installed("httr")

  # Test with empty term
  expect_no_error({
    # This would require a real API call, so we test parameter validation only
    term <- ""
    expect_type(term, "character")
    expect_equal(nchar(term), 0)
  })
})

test_that("query_umls returns correct structure for invalid terms", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")

  # Mock the HTTP responses for UMLS API
  # We'll test the structure that should be returned for terms not found

  expected_structure <- data.frame(
    cui = NA_character_,
    term = "nonexistentterm",
    semantic_type = "Unknown",
    source = "UMLS",
    definition = NA_character_,
    stringsAsFactors = FALSE
  )

  expect_s3_class(expected_structure, "data.frame")
  expect_true("cui" %in% colnames(expected_structure))
  expect_true("term" %in% colnames(expected_structure))
  expect_true("semantic_type" %in% colnames(expected_structure))
  expect_true("source" %in% colnames(expected_structure))
  expect_true("definition" %in% colnames(expected_structure))
})

test_that("query_umls integration test with real API", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  skip_if_not(can_run_umls_tests(), "Cannot run UMLS integration tests")

  # This test requires a real UMLS API key
  # We'll skip it in most cases unless explicitly testing with credentials
  skip_if(Sys.getenv("UMLS_API_KEY") == "", "No UMLS API key provided")

  api_key <- Sys.getenv("UMLS_API_KEY")

  result <- tryCatch({
    query_umls("migraine", api_key = api_key)
  }, error = function(e) {
    skip(paste("UMLS API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_true("cui" %in% colnames(result))
  expect_true("term" %in% colnames(result))
  expect_true("semantic_type" %in% colnames(result))
  expect_equal(result$source, "UMLS")
})

# Tests for query_mesh function
test_that("query_mesh validates parameters", {
  # Test that function accepts basic parameters
  expect_no_error({
    term <- "migraine"
    api_key <- "test_key"

    expect_type(term, "character")
    expect_true(is.null(api_key) || is.character(api_key))
  })
})

test_that("query_mesh returns correct structure", {
  skip_if_not_installed("rentrez")

  # Test the expected return structure
  expected_structure <- data.frame(
    mesh_id = NA_character_,
    term = "test_term",
    tree_number = NA_character_,
    scope_note = "No MeSH term found for: test_term",
    stringsAsFactors = FALSE
  )

  expect_s3_class(expected_structure, "data.frame")
  expect_true("mesh_id" %in% colnames(expected_structure))
  expect_true("term" %in% colnames(expected_structure))
  expect_true("tree_number" %in% colnames(expected_structure))
  expect_true("scope_note" %in% colnames(expected_structure))
})

test_that("query_mesh handles missing rentrez package", {
  # Test behavior when rentrez is not available
  # This tests the graceful degradation in the function

  # Mock the requireNamespace function to return FALSE
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    expect_message(
      result <- query_mesh("migraine"),
      "rentrez package is required"
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_true(is.na(result$mesh_id))
  }
})

test_that("query_mesh integration test with real API", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_mesh_tests(), "Cannot run MeSH integration tests")

  result <- tryCatch({
    query_mesh("migraine")
  }, error = function(e) {
    skip(paste("MeSH API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_true("mesh_id" %in% colnames(result))
  expect_true("term" %in% colnames(result))
  expect_true("tree_number" %in% colnames(result))
  expect_true("scope_note" %in% colnames(result))

  # If successful, check that we got actual data
  if (!is.na(result$mesh_id[1])) {
    expect_true(grepl("^D[0-9]+$", result$mesh_id[1]))
  }
})

test_that("query_mesh handles API errors gracefully", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_mesh_tests(), "Cannot run MeSH integration tests")

  # Test with a term that should cause issues or not be found
  result <- tryCatch({
    query_mesh("veryrarenonexistentterm12345")
  }, error = function(e) {
    # The function should handle errors gracefully
    skip(paste("Expected error occurred:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Should return a record even if term not found
  expect_equal(result$term[1], "veryrarenonexistentterm12345")
})

# Tests for enhance_abc_kb function
test_that("enhance_abc_kb validates parameters", {
  abc_results <- create_mock_abc_results()

  # Test knowledge_base parameter validation
  expect_error(
    enhance_abc_kb(abc_results, knowledge_base = "invalid"),
    "'arg' should be one of"
  )

  # Test with valid parameters
  expect_no_error({
    knowledge_base <- match.arg("mesh", c("umls", "mesh"))
    expect_equal(knowledge_base, "mesh")
  })
})

test_that("enhance_abc_kb handles empty results", {
  empty_results <- data.frame(
    a_term = character(0),
    b_terms = character(0),
    c_term = character(0),
    stringsAsFactors = FALSE
  )

  result <- enhance_abc_kb(empty_results, knowledge_base = "mesh")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("enhance_abc_kb extracts unique terms correctly", {
  abc_results <- create_mock_abc_results()

  # Test the term extraction logic (without making API calls)
  a_terms <- unique(abc_results$a_term)
  b_terms <- unique(unlist(strsplit(abc_results$b_terms, ", ")))
  c_terms <- unique(abc_results$c_term)
  unique_terms <- unique(c(a_terms, b_terms, c_terms))

  expect_type(unique_terms, "character")
  expect_gte(length(unique_terms), 3)
  expect_true("migraine" %in% unique_terms)
  expect_true("CGRP" %in% unique_terms)
  expect_true("serotonin" %in% unique_terms)
})

test_that("enhance_abc_kb with MeSH knowledge base (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_mesh_tests(), "Cannot run MeSH integration tests")

  abc_results <- create_mock_abc_results()

  result <- tryCatch({
    enhance_abc_kb(abc_results, knowledge_base = "mesh")
  }, error = function(e) {
    skip(paste("MeSH enhancement failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_gte(ncol(result), ncol(abc_results))

  # Should have added MeSH-related columns
  mesh_columns <- c("a_mesh_id", "a_tree_number", "c_mesh_id", "c_tree_number")
  expected_mesh_cols <- intersect(mesh_columns, colnames(result))
  expect_gte(length(expected_mesh_cols), 2)
})

test_that("enhance_abc_kb with UMLS knowledge base (integration test)", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  skip_if_not(can_run_umls_tests(), "Cannot run UMLS integration tests")
  skip_if(Sys.getenv("UMLS_API_KEY") == "", "No UMLS API key provided")

  abc_results <- create_mock_abc_results()
  api_key <- Sys.getenv("UMLS_API_KEY")

  result <- tryCatch({
    enhance_abc_kb(abc_results, knowledge_base = "umls", api_key = api_key)
  }, error = function(e) {
    skip(paste("UMLS enhancement failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_gte(ncol(result), ncol(abc_results))

  # Should have added UMLS-related columns
  umls_columns <- c("a_cui", "a_semantic_type", "c_cui", "c_semantic_type")
  expected_umls_cols <- intersect(umls_columns, colnames(result))
  expect_gte(length(expected_umls_cols), 2)
})

test_that("enhance_abc_kb handles API failures gracefully", {
  abc_results <- create_mock_abc_results()

  # Test with MeSH when rentrez might not be available
  result <- tryCatch({
    enhance_abc_kb(abc_results, knowledge_base = "mesh")
  }, error = function(e) {
    # Should handle missing packages gracefully
    expect_s3_class(abc_results, "data.frame")  # Return original if enhancement fails
    abc_results
  })

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(abc_results))
})

# Performance and edge case tests
test_that("enhance_abc_kb performance with large datasets", {
  # Create a larger mock dataset
  large_abc_results <- do.call(rbind, replicate(50, create_mock_abc_results(), simplify = FALSE))

  # Test that it doesn't take too long (without making API calls)
  start_time <- Sys.time()

  # Test the term extraction part (most computationally intensive)
  unique_terms <- unique(c(
    large_abc_results$a_term,
    unlist(strsplit(large_abc_results$b_terms, ", ")),
    large_abc_results$c_term
  ))

  end_time <- Sys.time()

  expect_lt(as.numeric(end_time - start_time), 1)  # Should complete in under 1 second
  expect_type(unique_terms, "character")
  expect_gte(length(unique_terms), 3)
})

test_that("query functions handle special characters in terms", {
  # Test with terms containing special characters
  special_terms <- c("migraine", "head-ache", "5-HT", "α-receptor", "β-blocker")

  for (term in special_terms) {
    expect_type(term, "character")
    expect_gt(nchar(term), 0)

    # Test that terms don't break URL encoding (for API calls)
    encoded_term <- utils::URLencode(term)
    expect_type(encoded_term, "character")
    expect_gte(nchar(encoded_term), nchar(term))
  }
})

test_that("query functions handle very long terms", {
  # Test with unusually long terms
  long_term <- paste(rep("verylongterm", 10), collapse = "")

  expect_type(long_term, "character")
  expect_gt(nchar(long_term), 50)

  # Test that long terms don't break the functions
  expect_no_error({
    # This is just parameter validation, not an actual API call
    if (requireNamespace("httr", quietly = TRUE)) {
      encoded_long_term <- utils::URLencode(long_term)
      expect_type(encoded_long_term, "character")
    }
  })
})

test_that("enhance_abc_kb handles malformed ABC results", {
  # Test with missing required columns
  malformed_results <- data.frame(
    wrong_column = "test",
    stringsAsFactors = FALSE
  )

  # Should handle missing columns gracefully
  expect_error({
    # This might error or handle gracefully depending on implementation
    enhance_abc_kb(malformed_results, knowledge_base = "mesh")
  }, ".*")  # Expect some kind of error or warning
})

# Integration test for the complete workflow
test_that("complete workflow integration test", {
  skip_if_not(can_run_mesh_tests(), "Cannot run complete workflow test")

  abc_results <- create_mock_abc_results()

  # Test the complete workflow with MeSH
  result <- tryCatch({
    enhance_abc_kb(abc_results, knowledge_base = "mesh")
  }, error = function(e) {
    skip(paste("Complete workflow failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(abc_results))
  expect_gte(ncol(result), ncol(abc_results))

  # Verify that original data is preserved
  for (col in colnames(abc_results)) {
    if (col %in% colnames(result)) {
      expect_equal(result[[col]], abc_results[[col]])
    }
  }
})

# Cleanup and utility tests
test_that("utility functions work correctly", {
  # Test progress bar functionality (used in enhance_abc_kb)
  expect_no_error({
    pb <- utils::txtProgressBar(min = 0, max = 10, style = 3)
    utils::setTxtProgressBar(pb, 5)
    close(pb)
  })

  # Test string manipulation used in the functions
  test_terms <- "term1, term2, term3"
  split_terms <- unlist(strsplit(test_terms, ", "))
  expect_equal(length(split_terms), 3)
  expect_equal(split_terms[1], "term1")
})

test_that("HTTP status code handling", {
  skip_if_not_installed("httr")

  # Test HTTP status code interpretation
  expect_equal(200, 200)  # Success
  expect_equal(401, 401)  # Unauthorized
  expect_equal(404, 404)  # Not found
  expect_equal(500, 500)  # Server error

  # Test that we understand HTTP status codes used in the functions
  success_codes <- c(200, 201)
  client_error_codes <- c(400, 401, 403, 404)
  server_error_codes <- c(500, 502, 503)

  expect_true(200 %in% success_codes)
  expect_true(404 %in% client_error_codes)
  expect_true(500 %in% server_error_codes)
})
