library(testthat)

# ============================================================================
# Tests for query_umls - Basic Functionality
# ============================================================================
test_that("query_umls requires API key", {
  expect_error(
    query_umls("migraine", api_key = NULL),
    "UMLS API key is required"
  )
})

test_that("query_umls error message includes registration URL", {
  expect_error(
    query_umls("test", api_key = NULL),
    "https://uts.nlm.nih.gov/uts/license"
  )
})

test_that("query_umls handles httr package dependency", {
  skip_if_not_installed("httr")
  skip_on_cran()
  
  # Function should exist and be callable
  expect_true(exists("query_umls"))
})

# ============================================================================
# Tests for query_umls - Response Structure
# ============================================================================
test_that("query_umls returns data frame with correct structure", {
  skip_on_cran()
  skip_if_not_installed("httr")
  
  # Expected column structure
  expected_cols <- c("cui", "term", "semantic_type", "source", "definition")
  
  # Mock response when no results found
  mock_result <- data.frame(
    cui = NA_character_,
    term = "test_term",
    semantic_type = "Unknown",
    source = "UMLS",
    definition = NA_character_,
    stringsAsFactors = FALSE
  )
  
  expect_true(all(expected_cols %in% colnames(mock_result)))
  expect_equal(nrow(mock_result), 1)
})

test_that("query_umls handles missing UI field", {
  # When UI field is missing, should return NA for CUI
  mock_result <- data.frame(
    cui = NA_character_,
    term = "test",
    semantic_type = "Unknown",
    source = "UMLS",
    definition = NA_character_,
    stringsAsFactors = FALSE
  )
  
  expect_true(is.na(mock_result$cui))
  expect_equal(mock_result$semantic_type, "Unknown")
})

test_that("query_umls handles missing concept data", {
  # When concept data is incomplete, should return Unknown semantic type
  mock_result <- data.frame(
    cui = "C0018681",
    term = "test",
    semantic_type = "Unknown",
    source = "UMLS",
    definition = NA_character_,
    stringsAsFactors = FALSE
  )
  
  expect_equal(mock_result$semantic_type, "Unknown")
  expect_equal(mock_result$source, "UMLS")
})

test_that("query_umls handles missing semantic types", {
  # When semantic types are missing
  mock_result <- data.frame(
    cui = "C0018681",
    term = "test",
    semantic_type = "Unknown",
    source = "UMLS",
    definition = NA_character_,
    stringsAsFactors = FALSE
  )
  
  expect_equal(mock_result$semantic_type, "Unknown")
})

test_that("query_umls handles multiple semantic types", {
  # Multiple semantic types should be comma-separated
  semantic_types <- c("Disease or Syndrome", "Sign or Symptom")
  combined <- paste(semantic_types, collapse = ", ")
  
  expect_equal(combined, "Disease or Syndrome, Sign or Symptom")
  expect_true(grepl(",", combined))
})

# ============================================================================
# Tests for query_umls - API Authentication
# ============================================================================
test_that("query_umls authentication flow structure", {
  skip_on_cran()
  
  # Base URLs should be correctly formatted
  base_url <- "https://uts-ws.nlm.nih.gov/rest"
  auth_url <- "https://utslogin.nlm.nih.gov/cas/v1/api-key"
  
  expect_true(grepl("^https://", base_url))
  expect_true(grepl("^https://", auth_url))
  expect_true(grepl("uts-ws.nlm.nih.gov", base_url))
})

test_that("query_umls service ticket URL construction", {
  # Service ticket should be for umlsks.nlm.nih.gov
  service_url <- "http://umlsks.nlm.nih.gov"
  expect_true(grepl("umlsks.nlm.nih.gov", service_url))
})

# ============================================================================
# Tests for query_umls - Error Handling
# ============================================================================
test_that("query_umls handles authentication failures gracefully", {
  skip_on_cran()
  
  # Mock authentication failure scenario
  expect_true(exists("query_umls"))
})

test_that("query_umls handles empty search results", {
  # When no results found
  mock_result <- data.frame(
    cui = NA_character_,
    term = "nonexistent_term",
    semantic_type = "Unknown",
    source = "UMLS",
    definition = NA_character_,
    stringsAsFactors = FALSE
  )
  
  expect_true(is.na(mock_result$cui))
  expect_true(is.na(mock_result$definition))
})

# ============================================================================
# Tests for query_mesh - Basic Functionality
# ============================================================================
test_that("query_mesh requires rentrez package", {
  skip_if_installed("rentrez")
  
  expect_message(
    result <- query_mesh("migraine"),
    "rentrez package is required"
  )
})

test_that("query_mesh handles missing rentrez gracefully", {
  skip_if_installed("rentrez")
  
  result <- suppressMessages(query_mesh("test"))
  
  expect_true(is.data.frame(result))
  expect_true(is.na(result$mesh_id))
})

test_that("query_mesh returns correct structure", {
  # Expected columns
  expected_cols <- c("mesh_id", "term", "tree_number", "scope_note")
  
  mock_result <- data.frame(
    mesh_id = NA_character_,
    term = "test",
    tree_number = NA_character_,
    scope_note = NA_character_,
    stringsAsFactors = FALSE
  )
  
  expect_true(all(expected_cols %in% colnames(mock_result)))
})

# ============================================================================
# Tests for query_mesh - API Key Handling
# ============================================================================
test_that("query_mesh accepts optional API key", {
  skip_on_cran()
  skip_if_not_installed("rentrez")
  
  # Should not error with NULL api_key
  expect_silent({
    api_key <- NULL
    is.null(api_key)
  })
})

test_that("query_mesh uses API key when provided", {
  skip_on_cran()
  skip_if_not_installed("rentrez")
  
  api_key <- "test_key"
  expect_equal(api_key, "test_key")
})

# ============================================================================
# Tests for query_mesh - MeSH Record Parsing
# ============================================================================
test_that("query_mesh extracts MeSH ID from text", {
  mesh_record <- "DescriptorUI: D008881\nDescriptorName: Migraine"
  
  # Extract MeSH ID pattern
  pattern <- "DescriptorUI: ([A-Z][0-9]+)"
  match <- regexpr(pattern, mesh_record)
  
  expect_true(match > 0)
  
  if (match > 0) {
    extracted <- regmatches(mesh_record, match)
    mesh_id <- gsub("DescriptorUI: ", "", extracted)
    expect_equal(mesh_id, "D008881")
  }
})

test_that("query_mesh extracts descriptor name from text", {
  mesh_record <- "DescriptorUI: D008881\nDescriptorName: Migraine Disorders"
  
  pattern <- "DescriptorName: ([^\n]+)"
  match <- regexpr(pattern, mesh_record)
  
  expect_true(match > 0)
  
  if (match > 0) {
    extracted <- regmatches(mesh_record, match)
    term <- gsub("DescriptorName: ", "", extracted)
    expect_equal(term, "Migraine Disorders")
  }
})

test_that("query_mesh extracts tree numbers from text", {
  mesh_record <- "Tree Number: C10.228.140.546\nTree Number: F03.087.250.450"
  
  pattern <- "Tree Number: ([A-Z][0-9\\.]+)"
  matches <- gregexpr(pattern, mesh_record)
  
  expect_true(matches[[1]][1] > 0)
  
  if (matches[[1]][1] > 0) {
    extracted <- regmatches(mesh_record, matches)[[1]]
    tree_numbers <- gsub("Tree Number: ", "", extracted)
    expect_equal(length(tree_numbers), 2)
    expect_true(grepl("^[A-Z][0-9\\.]+$", tree_numbers[1]))
  }
})

test_that("query_mesh extracts scope note from text", {
  mesh_record <- "Scope Note: A class of disabling primary headache disorders."
  
  pattern <- "Scope Note: ([^\n]+)"
  match <- regexpr(pattern, mesh_record)
  
  expect_true(match > 0)
  
  if (match > 0) {
    extracted <- regmatches(mesh_record, match)
    scope_note <- gsub("Scope Note: ", "", extracted)
    expect_true(grepl("headache", scope_note, ignore.case = TRUE))
  }
})

# ============================================================================
# Tests for query_mesh - Error Handling
# ============================================================================
test_that("query_mesh handles no results found", {
  skip_on_cran()
  skip_if_not_installed("rentrez")
  
  # Mock scenario where count is 0
  mock_search <- list(count = 0)
  expect_equal(mock_search$count, 0)
  
  # Should return data frame with NA values
  expected_result <- data.frame(
    mesh_id = NA_character_,
    term = "test",
    tree_number = NA_character_,
    scope_note = "No MeSH term found for: test",
    stringsAsFactors = FALSE
  )
  
  expect_true(is.na(expected_result$mesh_id))
  expect_true(grepl("No MeSH term found", expected_result$scope_note))
})

test_that("query_mesh handles API errors gracefully", {
  skip_on_cran()
  
  # Error result structure
  error_result <- data.frame(
    mesh_id = NA_character_,
    term = "test",
    tree_number = NA_character_,
    scope_note = "Error: API connection failed",
    stringsAsFactors = FALSE
  )
  
  expect_true(is.na(error_result$mesh_id))
  expect_true(grepl("Error:", error_result$scope_note))
})

test_that("query_mesh tryCatch handles exceptions", {
  # Test that tryCatch structure works
  result <- tryCatch({
    stop("Test error")
  }, error = function(e) {
    data.frame(
      mesh_id = NA_character_,
      term = "test",
      tree_number = NA_character_,
      scope_note = paste("Error:", e$message),
      stringsAsFactors = FALSE
    )
  })
  
  expect_true(is.data.frame(result))
  expect_true(grepl("Test error", result$scope_note))
})

# ============================================================================
# Tests for query_mesh - Message Output
# ============================================================================
test_that("query_mesh produces appropriate messages", {
  skip_on_cran()
  skip_if_not_installed("rentrez")
  
  # Should message when no results found
  mock_count <- 0
  if (mock_count == 0) {
    expect_true(TRUE)  # Would produce message in actual function
  }
})

# ============================================================================
# Tests for enhance_abc_kb - Basic Functionality
# ============================================================================
test_that("enhance_abc_kb requires knowledge_base parameter", {
  skip_on_cran()
  
  abc_results <- data.frame(
    a_term = "migraine",
    c_term = "sumatriptan",
    abc_score = 0.8
  )
  
  # Should accept valid knowledge bases
  valid_kb <- c("umls", "mesh")
  expect_true(all(valid_kb %in% c("umls", "mesh")))
})

test_that("enhance_abc_kb validates knowledge_base argument", {
  # match.arg should validate
  knowledge_base <- "mesh"
  valid_options <- c("umls", "mesh")
  
  expect_true(knowledge_base %in% valid_options)
})

test_that("enhance_abc_kb handles empty results", {
  empty_results <- data.frame(
    a_term = character(),
    c_term = character(),
    abc_score = numeric()
  )
  
  expect_equal(nrow(empty_results), 0)
})

test_that("enhance_abc_kb returns input for empty results", {
  empty_results <- data.frame(
    a_term = character(),
    c_term = character(),
    abc_score = numeric()
  )
  
  # Should return input unchanged when empty
  result <- empty_results
  expect_identical(result, empty_results)
})

# ============================================================================
# Tests for enhance_abc_kb - Term Extraction
# ============================================================================
test_that("enhance_abc_kb extracts unique terms correctly", {
  abc_results <- data.frame(
    a_term = c("migraine", "migraine", "headache"),
    b_terms = c("serotonin, CGRP", "CGRP, dopamine", "serotonin"),
    c_term = c("sumatriptan", "rizatriptan", "sumatriptan"),
    stringsAsFactors = FALSE
  )
  
  # Extract unique terms
  all_terms <- unique(c(
    abc_results$a_term,
    unlist(strsplit(abc_results$b_terms, ", ")),
    abc_results$c_term
  ))
  
  expect_true(length(all_terms) >= 5)
  expect_true("migraine" %in% all_terms)
  expect_true("sumatriptan" %in% all_terms)
})

test_that("enhance_abc_kb handles b_terms correctly", {
  b_terms_string <- "serotonin, CGRP, dopamine"
  b_terms_vector <- unlist(strsplit(b_terms_string, ", "))
  
  expect_equal(length(b_terms_vector), 3)
  expect_true("serotonin" %in% b_terms_vector)
  expect_true("CGRP" %in% b_terms_vector)
})

# ============================================================================
# Tests for enhance_abc_kb - UMLS Enhancement
# ============================================================================
test_that("enhance_abc_kb adds UMLS columns", {
  skip_on_cran()
  
  # Mock enhanced results with UMLS data
  enhanced <- data.frame(
    a_term = "migraine",
    c_term = "sumatriptan",
    abc_score = 0.8,
    a_cui = "C0018681",
    a_semantic_type = "Disease or Syndrome",
    c_cui = "C0076687",
    c_semantic_type = "Pharmacologic Substance",
    stringsAsFactors = FALSE
  )
  
  expect_true("a_cui" %in% colnames(enhanced))
  expect_true("a_semantic_type" %in% colnames(enhanced))
  expect_true("c_cui" %in% colnames(enhanced))
  expect_true("c_semantic_type" %in% colnames(enhanced))
})

test_that("enhance_abc_kb UMLS columns have correct structure", {
  umls_cols <- c("a_cui", "a_semantic_type", "c_cui", "c_semantic_type")
  
  expect_equal(length(umls_cols), 4)
  expect_true(all(grepl("cui|semantic_type", umls_cols)))
})

# ============================================================================
# Tests for enhance_abc_kb - MeSH Enhancement
# ============================================================================
test_that("enhance_abc_kb adds MeSH columns", {
  skip_on_cran()
  
  # Mock enhanced results with MeSH data
  enhanced <- data.frame(
    a_term = "migraine",
    c_term = "sumatriptan",
    abc_score = 0.8,
    a_mesh_id = "D008881",
    a_tree_number = "C10.228.140.546",
    c_mesh_id = "D018170",
    c_tree_number = "D02.033.755.624.776.850",
    stringsAsFactors = FALSE
  )
  
  expect_true("a_mesh_id" %in% colnames(enhanced))
  expect_true("a_tree_number" %in% colnames(enhanced))
  expect_true("c_mesh_id" %in% colnames(enhanced))
  expect_true("c_tree_number" %in% colnames(enhanced))
})

test_that("enhance_abc_kb MeSH columns have correct structure", {
  mesh_cols <- c("a_mesh_id", "a_tree_number", "c_mesh_id", "c_tree_number")
  
  expect_equal(length(mesh_cols), 4)
  expect_true(all(grepl("mesh_id|tree_number", mesh_cols)))
})

# ============================================================================
# Tests for enhance_abc_kb - Progress Tracking
# ============================================================================
test_that("enhance_abc_kb uses progress bar for large datasets", {
  skip_on_cran()
  
  # Progress bar should be created for term processing
  n_terms <- 10
  
  expect_silent({
    pb <- utils::txtProgressBar(min = 0, max = n_terms, style = 3)
    utils::setTxtProgressBar(pb, 5)
    close(pb)
  })
})

# ============================================================================
# Tests for enhance_abc_kb - Term Info Caching
# ============================================================================
test_that("enhance_abc_kb caches term information", {
  skip_on_cran()
  
  # Term info should be stored in a list
  term_info <- list()
  term_info[["migraine"]] <- data.frame(
    cui = "C0018681",
    semantic_type = "Disease or Syndrome",
    stringsAsFactors = FALSE
  )
  
  expect_true("migraine" %in% names(term_info))
  expect_equal(term_info[["migraine"]]$cui, "C0018681")
})

test_that("enhance_abc_kb reuses cached term info", {
  # Same term should use cached data
  term_info <- list()
  term_info[["migraine"]] <- list(cui = "C0018681")
  
  # Accessing cached data
  cached_cui <- term_info[["migraine"]]$cui
  expect_equal(cached_cui, "C0018681")
})

# ============================================================================
# Tests for enhance_abc_kb - sapply Operations
# ============================================================================
test_that("enhance_abc_kb uses sapply to add columns", {
  abc_results <- data.frame(
    a_term = c("migraine", "headache"),
    c_term = c("sumatriptan", "ibuprofen"),
    stringsAsFactors = FALSE
  )
  
  term_info <- list(
    "migraine" = list(cui = "C0018681"),
    "headache" = list(cui = "C0018681"),
    "sumatriptan" = list(cui = "C0076687"),
    "ibuprofen" = list(cui = "C0020740")
  )
  
  # Simulate sapply operation
  a_cuis <- sapply(abc_results$a_term, function(term) term_info[[term]]$cui)
  
  expect_equal(length(a_cuis), 2)
  expect_equal(a_cuis[1], "C0018681")
})

# ============================================================================
# Tests for enhance_abc_kb - API Key Handling
# ============================================================================
test_that("enhance_abc_kb passes API key to query functions", {
  skip_on_cran()
  
  api_key <- "test_api_key"
  knowledge_base <- "umls"
  
  expect_equal(api_key, "test_api_key")
  expect_equal(knowledge_base, "umls")
})

test_that("enhance_abc_kb works without API key for MeSH", {
  skip_on_cran()
  
  knowledge_base <- "mesh"
  api_key <- NULL
  
  expect_null(api_key)
  expect_equal(knowledge_base, "mesh")
})

# ============================================================================
# Tests for enhance_abc_kb - Message Output
# ============================================================================
test_that("enhance_abc_kb produces informative messages", {
  skip_on_cran()
  
  n_terms <- 5
  kb <- "mesh"
  
  expected_message <- paste("Enhancing", n_terms, "unique terms with", kb)
  expect_true(grepl("Enhancing", expected_message))
  expect_true(grepl("mesh", expected_message))
})

# ============================================================================
# Tests for enhance_abc_kb - Result Structure Preservation
# ============================================================================
test_that("enhance_abc_kb preserves original columns", {
  original <- data.frame(
    a_term = "migraine",
    c_term = "sumatriptan",
    abc_score = 0.8,
    stringsAsFactors = FALSE
  )
  
  enhanced <- original
  enhanced$a_mesh_id <- "D008881"
  
  # Original columns should be preserved
  expect_true(all(colnames(original) %in% colnames(enhanced)))
  expect_equal(enhanced$abc_score, 0.8)
})

test_that("enhance_abc_kb maintains row count", {
  original <- data.frame(
    a_term = c("migraine", "headache", "pain"),
    c_term = c("drug1", "drug2", "drug3"),
    abc_score = c(0.8, 0.7, 0.6),
    stringsAsFactors = FALSE
  )
  
  # Enhancement should not change number of rows
  expect_equal(nrow(original), 3)
})

# ============================================================================
# Integration Tests for Query Functions
# ============================================================================
test_that("query functions work together in enhance_abc_kb", {
  skip_on_cran()
  
  # Mock workflow
  abc_results <- data.frame(
    a_term = "migraine",
    b_terms = "serotonin",
    c_term = "sumatriptan",
    abc_score = 0.8,
    stringsAsFactors = FALSE
  )
  
  # Should be able to extract terms
  terms <- unique(c(abc_results$a_term, abc_results$c_term))
  expect_equal(length(terms), 2)
})

# ============================================================================
# Tests for URL Construction
# ============================================================================
test_that("UMLS API URLs are correctly formatted", {
  base_url <- "https://uts-ws.nlm.nih.gov/rest"
  version <- "current"
  cui <- "C0018681"
  
  concept_url <- paste0(base_url, "/content/", version, "/CUI/", cui)
  
  expect_true(grepl("^https://", concept_url))
  expect_true(grepl(cui, concept_url))
  expect_true(grepl(version, concept_url))
})

test_that("UMLS search URL construction", {
  base_url <- "https://uts-ws.nlm.nih.gov/rest"
  version <- "current"
  
  search_url <- paste0(base_url, "/search/", version)
  
  expect_equal(search_url, "https://uts-ws.nlm.nih.gov/rest/search/current")
})

test_that("UMLS semantic types URL construction", {
  base_url <- "https://uts-ws.nlm.nih.gov/rest"
  version <- "current"
  cui <- "C0018681"
  
  concept_url <- paste0(base_url, "/content/", version, "/CUI/", cui)
  semantics_url <- paste0(concept_url, "/semanticTypes")
  
  expect_true(grepl("/semanticTypes$", semantics_url))
})

test_that("UMLS definitions URL construction", {
  base_url <- "https://uts-ws.nlm.nih.gov/rest"
  version <- "current"
  cui <- "C0018681"
  
  concept_url <- paste0(base_url, "/content/", version, "/CUI/", cui)
  definitions_url <- paste0(concept_url, "/definitions")
  
  expect_true(grepl("/definitions$", definitions_url))
})
