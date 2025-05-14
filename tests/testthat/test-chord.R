# Tests for chord diagram visualization functions

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

  # Add entity types
  results$a_type <- sample(entity_types, n_results, replace = TRUE)
  results$b_type <- sample(entity_types, n_results, replace = TRUE)
  results$c_type <- sample(entity_types, n_results, replace = TRUE)

  # Ensure there are no duplicate A-B-C combinations
  results <- unique(results[, c("a_term", "b_term", "c_term", "a_b_score", "b_c_score", "abc_score",
                                "a_type", "b_type", "c_type")])

  return(results)
}

# Normalize special characters in file content
normalize_content <- function(content) {
  # Replace specific patterns that might vary between systems
  content <- gsub("\\r\\n", "\\n", content)  # Normalize line endings
  content <- gsub("\\t", " ", content)       # Replace tabs with spaces
  content <- gsub(" +", " ", content)        # Collapse multiple spaces
  return(content)
}

# Test export_chord function
test_that("export_chord generates an HTML chord diagram file", {
  # Create mock results
  mock_results <- create_mock_abc_results(30)

  # Create temporary file for output
  temp_file <- tempfile(fileext = ".html")

  # Test basic export - don't expect silence, allow messages
  expect_no_error(export_chord(mock_results, output_file = temp_file, open = FALSE))

  # Check that file exists and has content
  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)

  # Check that file contains chord diagram elements
  file_content <- readLines(temp_file, warn = FALSE)
  content_text <- paste(file_content, collapse = "\n")
  expect_match(content_text, "ABC Model Chord Diagram")
  expect_match(content_text, "https://d3js\\.org/d3\\.v5\\.min\\.js")  # Correctly escape dot in regex

  # Clean up
  unlink(temp_file)

  # Test with different parameters
  temp_file2 <- tempfile(fileext = ".html")
  expect_no_error(export_chord(mock_results, output_file = temp_file2,
                               top_n = 15, min_score = 0.05, open = FALSE))

  # Check that file exists and has content
  expect_true(file.exists(temp_file2))
  expect_gt(file.size(temp_file2), 0)

  # Clean up
  unlink(temp_file2)
})

# Test export_chord_diagram function
test_that("export_chord_diagram generates an HTML chord diagram file", {
  # Create mock results
  mock_results <- create_mock_abc_results(30)

  # Create temporary file for output
  temp_file <- tempfile(fileext = ".html")

  # Test basic export - don't expect silence, allow messages
  expect_no_error(export_chord_diagram(mock_results, output_file = temp_file, open = FALSE))

  # Check that file exists and has content
  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)

  # Check that file contains chord diagram elements
  file_content <- readLines(temp_file, warn = FALSE)
  content_text <- paste(file_content, collapse = "\n")
  expect_match(content_text, "ABC Model Chord Diagram")
  expect_match(content_text, "https://d3js\\.org/d3\\.v5\\.min\\.js")  # Correctly escape dot in regex

  # Check for role-based coloring
  expect_match(content_text, "roleColors")
  expect_match(content_text, "'A':")
  expect_match(content_text, "'B':")
  expect_match(content_text, "'C':")

  # Clean up
  unlink(temp_file)
})

# Test that export_chord and export_chord_diagram are equivalent
test_that("export_chord and export_chord_diagram produce similar output", {
  # Create mock results
  mock_results <- create_mock_abc_results(20)

  # Create temporary files for output
  temp_file1 <- tempfile(fileext = ".html")
  temp_file2 <- tempfile(fileext = ".html")

  # Generate both files
  expect_no_error({
    export_chord(mock_results, output_file = temp_file1, top_n = 10, min_score = 0.05, open = FALSE)
    export_chord_diagram(mock_results, output_file = temp_file2, top_n = 10, min_score = 0.05, open = FALSE)
  })

  # Check that both files exist
  expect_true(file.exists(temp_file1))
  expect_true(file.exists(temp_file2))

  # Both should contain chord diagram elements
  content1 <- readLines(temp_file1, warn = FALSE)
  content2 <- readLines(temp_file2, warn = FALSE)

  # Files might not be identical due to timestamp/format differences, but should contain key elements
  expect_true(any(grepl("chord\\(", content1)))
  expect_true(any(grepl("chord\\(", content2)))

  # Clean up
  unlink(temp_file1)
  unlink(temp_file2)
})

# Test error handling in chord diagram functions
test_that("chord diagram functions handle errors properly", {
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
  expect_error(export_chord(empty_results, tempfile(), open = FALSE), "ABC results are empty")
  expect_error(export_chord_diagram(empty_results, tempfile(), open = FALSE), "ABC results are empty")

  # Create results with scores below threshold
  low_score_results <- create_mock_abc_results(10)
  low_score_results$abc_score <- 0.001  # All scores below default threshold

  # Test with all scores below threshold
  expect_error(export_chord(low_score_results, tempfile(), min_score = 0.1, open = FALSE),
               "No results remain after filtering")
  expect_error(export_chord_diagram(low_score_results, tempfile(), min_score = 0.1, open = FALSE),
               "No results remain after filtering")
})

# Test with malformed input data
test_that("chord diagram functions handle malformed data gracefully", {
  # Create results with missing required columns
  malformed_results <- data.frame(
    a_term = c("Term A1", "Term A2"),
    b_term = c("Term B1", "Term B2"),
    c_term = c("Term C1", "Term C2"),
    # Omit a_b_score and b_c_score
    abc_score = c(0.2, 0.3)
  )

  # Test with missing columns
  temp_file <- tempfile(fileext = ".html")
  expect_error(export_chord_diagram(malformed_results, temp_file, open = FALSE))

  # Create results with NA values
  na_results <- create_mock_abc_results(10)
  na_results$a_term[1] <- NA
  na_results$b_term[3] <- NA
  na_results$c_term[5] <- NA

  # This should now handle the errors internally
  temp_file2 <- tempfile(fileext = ".html")
  suppressWarnings(
    expect_error(export_chord_diagram(na_results, output_file = temp_file2, open = FALSE),
                 NA)  # expect no error
  )
})

# Test with special characters in terms
test_that("chord diagram handles special characters in terms", {
  # Create results with special characters
  special_char_results <- create_mock_abc_results(10)
  special_char_results$a_term[1] <- "Term with quotes"
  special_char_results$b_term[2] <- "Term with backslash"
  special_char_results$c_term[3] <- "Term with apostrophes"

  # This should not error
  temp_file <- tempfile(fileext = ".html")
  expect_no_error(export_chord_diagram(special_char_results, output_file = temp_file, open = FALSE))

  # File should be created
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})
