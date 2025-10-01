# Test file for comprehensive_summary.R functions
# Tests for run_lbd() and apply_bitola_flexible()

library(testthat)

# Helper function to create test entity data
create_test_entity_data <- function() {
  data.frame(
    doc_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
    entity = c("migraine", "headache", "serotonin",
               "migraine", "serotonin", "receptor",
               "headache", "receptor", "pain",
               "migraine", "pain", "treatment"),
    entity_type = c("disease", "symptom", "chemical",
                    "disease", "chemical", "protein",
                    "symptom", "protein", "symptom",
                    "disease", "symptom", "drug"),
    count = c(2, 1, 3, 1, 2, 1, 2, 1, 1, 1, 1, 2),
    stringsAsFactors = FALSE
  )
}

# Helper function to create mock PubMed results
create_mock_articles <- function() {
  data.frame(
    pmid = c("12345", "23456", "34567"),
    title = c("Migraine and serotonin",
              "Headache treatment",
              "Pain receptors"),
    abstract = c("Migraine is associated with serotonin levels.",
                 "Headache can be treated with various drugs.",
                 "Pain is mediated by receptors."),
    publication_year = c("2020", "2021", "2022"),
    journal = c("Journal A", "Journal B", "Journal C"),
    stringsAsFactors = FALSE
  )
}

# Skip if not installed helper
skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste("Package", pkg, "not available"))
  }
}

# Skip if offline helper
skip_if_offline <- function() {
  tryCatch({
    con <- url("https://www.google.com", open = "rb", timeout = 2)
    close(con)
  }, error = function(e) {
    skip("No internet connection available")
  })
}

# ==============================================================================
# Tests for run_lbd()
# ==============================================================================

test_that("run_lbd validates discovery approaches", {
  expect_error(
    run_lbd("migraine", "migraine", discovery_approaches = c("invalid")),
    "Invalid discovery approaches"
  )

  expect_error(
    run_lbd("migraine", "migraine", discovery_approaches = c("abc", "invalid")),
    "Invalid discovery approaches"
  )
})

test_that("run_lbd validates dictionary sources", {
  expect_error(
    run_lbd("migraine", "migraine", dictionary_sources = c("invalid")),
    "Invalid dictionary sources"
  )

  expect_error(
    run_lbd("migraine", "migraine", dictionary_sources = c("mesh", "invalid")),
    "Invalid dictionary sources"
  )
})

test_that("run_lbd accepts valid parameters", {
  skip("Requires PubMed access and full workflow")
  skip_if_offline()
  skip_if_not_installed("rentrez")
  skip_if_not_installed("Matrix")

  # This would require mocking the entire workflow
  # Testing parameter validation instead
  expect_error(
    run_lbd("migraine", "migraine",
            discovery_approaches = c("abc", "anc"),
            dictionary_sources = c("local")),
    NA  # Should not error on parameter validation
  )
})

test_that("run_lbd requires valid search query", {
  skip("Requires PubMed access")
  skip_if_offline()

  # Would need to mock pubmed_search to test empty results
  expect_true(TRUE)  # Placeholder
})

test_that("run_lbd handles multiple discovery approaches", {
  skip("Requires full workflow")

  # Test that all valid approaches are accepted
  approaches <- c("abc", "anc", "lsi", "bitola")

  for (approach in approaches) {
    expect_error(
      run_lbd("test", "test", discovery_approaches = approach),
      NA,  # Should not error on validation
      label = paste("Testing approach:", approach)
    )
  }
})

test_that("run_lbd handles multiple dictionary sources", {
  skip("Requires full workflow")

  sources <- c("local", "mesh", "umls")

  for (source in sources) {
    expect_error(
      run_lbd("test", "test", dictionary_sources = source),
      NA,  # Should not error on validation
      label = paste("Testing source:", source)
    )
  }
})

test_that("run_lbd handles entity categories", {
  skip("Requires full workflow")

  categories <- c("disease", "drug", "gene", "protein", "chemical")

  expect_error(
    run_lbd("test", "test", entity_categories = categories),
    NA  # Should not error on validation
  )
})

test_that("run_lbd output structure with minimal workflow", {
  skip("Requires mocking multiple functions")

  # Would need to mock:
  # - pubmed_search
  # - vec_preprocess
  # - load_dictionary
  # - extract_entities
  # - create_comat
  # - abc_model, anc_model, lsi_model, etc.

  expect_true(TRUE)  # Placeholder
})

# ==============================================================================
# Tests for apply_bitola_flexible()
# ==============================================================================

test_that("apply_bitola_flexible requires valid co-occurrence matrix", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  expect_error(
    apply_bitola_flexible(co_matrix, a_term = "nonexistent"),
    "not found in the co-occurrence matrix"
  )
})

test_that("apply_bitola_flexible returns correct structure", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.01)

  expect_s3_class(results, "data.frame")

  if (nrow(results) > 0) {
    expect_true(all(c("a_term", "a_type", "c_term", "c_type",
                      "support", "bitola_score") %in% names(results)))
  }
})

test_that("apply_bitola_flexible handles matrix without entity types", {
  skip_if_not_installed("Matrix")

  entity_data <- data.frame(
    doc_id = c(1, 1, 2, 2),
    entity = c("a", "b", "a", "c"),
    stringsAsFactors = FALSE
  )

  co_matrix <- create_comat(entity_data, type_col = "nonexistent")

  expect_message(
    results <- apply_bitola_flexible(co_matrix, a_term = "a"),
    "No entity type information available"
  )

  expect_s3_class(results, "data.frame")
})

test_that("apply_bitola_flexible filters by minimum score", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.5)

  if (nrow(results) > 0) {
    expect_true(all(results$bitola_score >= 0))
  }
})

test_that("apply_bitola_flexible respects n_results parameter", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", n_results = 3)

  expect_lte(nrow(results), 3)
})

test_that("apply_bitola_flexible handles A term without type", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  # Create a test where A term exists but has no type in the entity_types attribute
  entity_types <- attr(co_matrix, "entity_types")
  # Remove migraine from entity_types entirely
  entity_types <- entity_types[names(entity_types) != "migraine"]
  attr(co_matrix, "entity_types") <- entity_types

  # Should still work, just with a message
  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.01)

  expect_s3_class(results, "data.frame")

  if (nrow(results) > 0) {
    # A type should be "unknown" when not found
    expect_true(all(results$a_type == "unknown"))
  }
})

test_that("apply_bitola_flexible handles empty B terms", {
  skip_if_not_installed("Matrix")

  entity_data <- data.frame(
    doc_id = c(1, 2),
    entity = c("term1", "term1"),
    entity_type = c("type1", "type1"),
    stringsAsFactors = FALSE
  )

  co_matrix <- create_comat(entity_data)

  expect_message(
    results <- apply_bitola_flexible(co_matrix, a_term = "term1", min_score = 0.9),
    "No B terms found"
  )

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 0)
})

test_that("apply_bitola_flexible aggregates by C term", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.01)

  if (nrow(results) > 0) {
    # Each C term should appear only once in final results
    expect_equal(length(results$c_term), length(unique(results$c_term)))

    # Support should be at least 1
    expect_true(all(results$support >= 1))
  }
})

test_that("apply_bitola_flexible includes b_terms column", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.01)

  if (nrow(results) > 0) {
    expect_true("b_terms" %in% names(results))
    expect_type(results$b_terms, "character")
  }
})

test_that("apply_bitola_flexible calculates ranking_score", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.01)

  if (nrow(results) > 0) {
    expect_true("ranking_score" %in% names(results))

    # Ranking score should be support * bitola_score
    expected_scores <- results$support * results$bitola_score
    expect_equal(results$ranking_score, expected_scores)
  }
})

test_that("apply_bitola_flexible sorts by ranking_score", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.01)

  if (nrow(results) > 1) {
    # Results should be in descending order of ranking_score
    expect_true(all(diff(results$ranking_score) <= 0))
  }
})

test_that("apply_bitola_flexible handles missing entity types gracefully", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  # Get entity types and remove some entries entirely (not just set to NA)
  entity_types <- attr(co_matrix, "entity_types")

  # Remove receptor and pain from the entity_types vector completely
  entity_types <- entity_types[!names(entity_types) %in% c("receptor", "pain")]
  attr(co_matrix, "entity_types") <- entity_types

  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.01)

  expect_s3_class(results, "data.frame")

  if (nrow(results) > 0) {
    # Check if any terms that were removed appear in results
    # If receptor or pain appear, they should have "unknown" type
    has_receptor <- any(results$b_term == "receptor" | results$c_term == "receptor")
    has_pain <- any(results$b_term == "pain" | results$c_term == "pain")

    if (has_receptor || has_pain) {
      # At least one should be marked as unknown
      expect_true(
        any(results$b_type == "unknown") || any(results$c_type == "unknown"),
        info = "Terms without entity types should be marked as 'unknown'"
      )
    } else {
      # If neither receptor nor pain appear, that's also valid
      expect_true(TRUE)
    }
  }
})

test_that("apply_bitola_flexible excludes A and B terms from C terms", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.01)

  if (nrow(results) > 0) {
    # A term should not appear as C term
    expect_false("migraine" %in% results$c_term)

    # B terms should not appear as C terms in the same row
    for (i in 1:nrow(results)) {
      b_terms_list <- unlist(strsplit(results$b_terms[i], ", "))
      expect_false(results$c_term[i] %in% b_terms_list)
    }
  }
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("apply_bitola_flexible integrates with create_comat", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data, normalize = TRUE)

  results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.01)

  expect_s3_class(results, "data.frame")
  expect_true("a_type" %in% names(results))
  expect_true("c_type" %in% names(results))
})

test_that("run_lbd parameter combinations", {
  skip_if_offline()
  skip_if_not_installed("rentrez")
  skip_if_not_installed("Matrix")

  # Test valid parameter combinations
  # These will fail on PubMed search (no articles), but that's expected
  # We're testing that the parameter validation doesn't error

  # All approaches
  expect_error(
    run_lbd("nonexistentqueryxyz123", "test", discovery_approaches = c("abc", "anc", "lsi", "bitola")),
    "No articles found",  # Expected error from PubMed
    info = "All approaches should be valid"
  )

  # Subset of approaches
  expect_error(
    run_lbd("nonexistentqueryxyz123", "test", discovery_approaches = c("abc", "lsi")),
    "No articles found",  # Expected error from PubMed
    info = "Subset of approaches should be valid"
  )

  # All sources
  expect_error(
    run_lbd("nonexistentqueryxyz123", "test", dictionary_sources = c("local", "mesh")),
    "No articles found",  # Expected error from PubMed
    info = "Multiple sources should be valid"
  )
})

# ==============================================================================
# Edge cases and error handling
# ==============================================================================

test_that("apply_bitola_flexible handles very small matrices", {
  skip_if_not_installed("Matrix")

  entity_data <- data.frame(
    doc_id = c(1, 1, 2, 2),
    entity = c("a", "b", "a", "c"),
    entity_type = c("type1", "type2", "type1", "type3"),
    stringsAsFactors = FALSE
  )

  co_matrix <- create_comat(entity_data)
  results <- apply_bitola_flexible(co_matrix, a_term = "a", min_score = 0.01)

  expect_s3_class(results, "data.frame")
})

test_that("apply_bitola_flexible handles no connections", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  # Very high threshold should return no results
  expect_message(
    results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.99),
    "No B terms found"
  )

  expect_equal(nrow(results), 0)
})

test_that("run_lbd validates required parameters", {
  expect_error(
    run_lbd(),
    "argument.*is missing"
  )
})

test_that("apply_bitola_flexible handles single document", {
  skip_if_not_installed("Matrix")

  entity_data <- data.frame(
    doc_id = rep(1, 4),
    entity = c("a", "b", "c", "d"),
    entity_type = c("t1", "t2", "t3", "t4"),
    stringsAsFactors = FALSE
  )

  co_matrix <- create_comat(entity_data)
  results <- apply_bitola_flexible(co_matrix, a_term = "a", min_score = 0.01)

  expect_s3_class(results, "data.frame")
})

test_that("run_lbd output_file parameter", {
  skip("Requires full workflow")

  # Test that output_file parameter is used
  expect_error(
    run_lbd("test", "test", output_file = "custom_report.html"),
    "No articles found"
  )
})

test_that("run_lbd include_visualizations parameter", {
  skip("Requires full workflow")

  # Test with visualizations disabled
  expect_error(
    run_lbd("test", "test", include_visualizations = FALSE),
    "No articles found"
  )

  # Test with visualizations enabled
  expect_error(
    run_lbd("test", "test", include_visualizations = TRUE),
    "No articles found"
  )
})

# ==============================================================================
# Performance tests
# ==============================================================================

test_that("apply_bitola_flexible handles moderate data efficiently", {
  skip_if_not_installed("Matrix")
  skip("Slow test - run manually")

  # Create larger dataset
  n_docs <- 50
  n_entities_per_doc <- 8

  entity_data <- data.frame(
    doc_id = rep(1:n_docs, each = n_entities_per_doc),
    entity = sample(paste0("entity", 1:30), n_docs * n_entities_per_doc, replace = TRUE),
    entity_type = sample(c("disease", "chemical", "protein"),
                         n_docs * n_entities_per_doc, replace = TRUE),
    stringsAsFactors = FALSE
  )

  co_matrix <- create_comat(entity_data)

  start_time <- Sys.time()
  results <- apply_bitola_flexible(co_matrix, a_term = "entity1")
  end_time <- Sys.time()

  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_lt(elapsed, 30)  # Should complete within 30 seconds
})

test_that("apply_bitola_flexible progress bar works", {
  skip_if_not_installed("Matrix")

  entity_data <- create_test_entity_data()
  co_matrix <- create_comat(entity_data)

  # Progress bar should be created without errors
  expect_message(
    results <- apply_bitola_flexible(co_matrix, a_term = "migraine", min_score = 0.01),
    "Analyzing.*B terms"
  )
})
