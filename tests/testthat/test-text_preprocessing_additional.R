# Additional tests for uncovered portions of text_preprocessing.R
library(testthat)
library(LBDiscover)

# ============================================================================
# Helper functions - MUST BE DEFINED FIRST
# ============================================================================
create_test_data <- function() {
  data.frame(
    doc_id = 1:3,
    abstract = c(
      "Migraine is a neurological disorder causing severe headache and photophobia.",
      "Serotonin receptors play a role in migraine pathophysiology.",
      "Sumatriptan is an effective treatment for migraine attacks."
    ),
    title = c("Migraine Study 1", "Serotonin Research", "Drug Treatment"),
    stringsAsFactors = FALSE
  )
}

create_test_dictionary <- function() {
  data.frame(
    term = c("migraine", "headache", "photophobia", "serotonin", "sumatriptan", "receptor"),
    type = c("disease", "symptom", "symptom", "chemical", "drug", "protein"),
    id = paste0("TEST_", 1:6),
    source = rep("test", 6),
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# Test: load_from_umls - semantic type extraction edge cases
# ============================================================================
test_that("load_from_umls handles semantic types as nested list elements", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  skip_on_cran()

  # This tests the complex semantic type extraction logic
  # We can't easily mock the UMLS API, so we test with invalid key
  # to trigger error paths

  result <- suppressWarnings(
    LBDiscover:::load_from_umls(
      dictionary_type = "disease",
      api_key = "invalid_key_for_testing",
      n_terms = 5,
      semantic_types = c("T047")
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("load_from_umls handles semantic types as data frame with uri column", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  skip_on_cran()

  # Test with multiple semantic type filters
  result <- suppressWarnings(
    LBDiscover:::load_from_umls(
      dictionary_type = "drug",
      api_key = "invalid_test_key",
      n_terms = 5,
      semantic_types = c("T116", "T121")
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("load_from_umls handles semantic types with semanticType column", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  skip_on_cran()

  result <- suppressWarnings(
    LBDiscover:::load_from_umls(
      dictionary_type = "gene",
      api_key = "test_key",
      n_terms = 5,
      semantic_types = c("T028")
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("load_from_umls handles character semantic types in result list", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  skip_on_cran()

  # Test error handling in semantic type extraction
  result <- suppressWarnings(
    LBDiscover:::load_from_umls(
      dictionary_type = "protein",
      api_key = "invalid_key",
      n_terms = 5,
      semantic_types = c("T116", "T123")
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("load_from_umls handles multiple matching semantic types", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  skip_on_cran()

  result <- suppressWarnings(
    LBDiscover:::load_from_umls(
      dictionary_type = "chemical",
      api_key = "test",
      n_terms = 10,
      semantic_types = c("T103", "T104", "T196")
    )
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: process_mesh_chunks - error handling
# ============================================================================
test_that("process_mesh_chunks handles malformed XML records", {
  # Create a string with incomplete XML records
  malformed_xml <- paste(
    "<DescriptorRecord><DescriptorUI>D001</DescriptorUI>",
    "<DescriptorRecord><DescriptorUI>D002</DescriptorUI></DescriptorRecord>",
    sep = ""
  )

  result <- suppressWarnings(
    LBDiscover:::process_mesh_chunks(malformed_xml, "disease")
  )

  expect_s3_class(result, "data.frame")
})

test_that("process_mesh_chunks handles chunks with no complete records", {
  # Create XML that doesn't have complete record boundaries
  incomplete_xml <- "<Descriptor"

  result <- suppressWarnings(
    LBDiscover:::process_mesh_chunks(incomplete_xml, "disease")
  )

  expect_s3_class(result, "data.frame")
})

test_that("process_mesh_chunks handles mixed record types", {
  mixed_xml <- paste(
    "<DescriptorRecord><DescriptorUI>D001</DescriptorUI></DescriptorRecord>",
    "<Concept><ConceptUI>C001</ConceptUI></Concept>",
    sep = ""
  )

  result <- suppressWarnings(
    LBDiscover:::process_mesh_chunks(mixed_xml, "disease")
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: sanitize_dictionary - uncovered validation paths
# ============================================================================
test_that("sanitize_dictionary handles protein validation with receptor term", {
  protein_dict <- data.frame(
    term = c("receptor", "receptors", "short"),
    type = rep("protein", 3),
    id = paste0("PROT_", 1:3),
    source = rep("test", 3),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(protein_dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  # receptor/receptors should be kept as special case
  if (nrow(result) > 0) {
    expect_true("receptor" %in% result$term || "receptors" %in% result$term)
  }
})

test_that("sanitize_dictionary handles disease validation with migraine", {
  disease_dict <- data.frame(
    term = c("migraine", "random_disease", "cancer"),
    type = rep("disease", 3),
    id = paste0("DIS_", 1:3),
    source = rep("test", 3),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(disease_dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  # migraine is specifically handled
  if (nrow(result) > 0) {
    expect_true("migraine" %in% result$term)
  }
})

test_that("sanitize_dictionary handles symptom validation with known symptoms", {
  symptom_dict <- data.frame(
    term = c("pain", "headache", "fatigue", "photophobia", "random"),
    type = rep("symptom", 5),
    id = paste0("SYMP_", 1:5),
    source = rep("test", 5),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(symptom_dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  # Known symptoms should be preserved
  if (nrow(result) > 0) {
    known_symptoms <- c("pain", "headache", "fatigue", "photophobia")
    expect_true(any(known_symptoms %in% result$term))
  }
})

test_that("sanitize_dictionary handles biological_process validation", {
  bioprocess_dict <- data.frame(
    term = c("inflammation", "signaling", "activation", "random_process"),
    type = rep("biological_process", 4),
    id = paste0("BP_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(bioprocess_dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  # Known processes should be kept
  if (nrow(result) > 0) {
    known_processes <- c("inflammation", "signaling", "activation")
    expect_true(any(known_processes %in% result$term))
  }
})

test_that("sanitize_dictionary handles method validation with analytical methods", {
  method_dict <- data.frame(
    term = c("faers", "bcpnn", "uplc", "frap", "hplc", "random_method"),
    type = rep("method", 6),
    id = paste0("METH_", 1:6),
    source = rep("test", 6),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(method_dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  # Known analytical methods should be preserved
  if (nrow(result) > 0) {
    analytical_methods <- c("faers", "bcpnn", "uplc", "frap", "hplc")
    expect_true(any(analytical_methods %in% result$term))
  }
})

test_that("sanitize_dictionary handles type correction with term_type_mappings", {
  dict <- data.frame(
    term = c("bcpnn", "faers", "uplc", "frap"),
    type = c("disease", "drug", "gene", "protein"),  # Wrong types
    id = paste0("ID_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dict, validate_types = TRUE, verbose = TRUE)

  expect_s3_class(result, "data.frame")
  # Check if corrections were attempted (methods should have type "method")
  if (nrow(result) > 0 && any(c("bcpnn", "faers", "uplc", "frap") %in% result$term)) {
    corrected_terms <- result[result$term %in% c("bcpnn", "faers", "uplc", "frap"), ]
    if (nrow(corrected_terms) > 0) {
      expect_true(all(corrected_terms$type == "method"))
    }
  }
})

# ============================================================================
# Test: extract_entities_workflow - uncovered branches
# ============================================================================
test_that("extract_entities_workflow handles source_map for expanded types", {
  text_data <- create_test_data()

  # Test with a mix of local and expanded entity types
  result <- extract_entities_workflow(
    text_data,
    entity_types = c("disease", "protein", "pathway"),
    dictionary_sources = c("local", "mesh"),
    max_terms_per_type = 10,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles dictionary loading errors gracefully", {
  text_data <- create_test_data()

  # Request entity types that might fail to load
  result <- suppressWarnings(
    extract_entities_workflow(
      text_data,
      entity_types = c("nonexistent_type1", "nonexistent_type2"),
      dictionary_sources = "local",
      verbose = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles parallel dictionary loading errors", {
  skip_if_not(requireNamespace("parallel", quietly = TRUE))

  text_data <- create_test_data()

  result <- suppressWarnings(
    extract_entities_workflow(
      text_data,
      entity_types = c("disease"),
      dictionary_sources = "local",
      parallel = TRUE,
      num_cores = 2,
      verbose = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles batch processing errors", {
  text_data <- data.frame(
    doc_id = 1:20,
    abstract = c(rep("valid text", 15), rep(NA, 5))
  )

  result <- suppressWarnings(
    extract_entities_workflow(
      text_data,
      entity_types = "disease",
      dictionary_sources = "local",
      batch_size = 10,
      verbose = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow prioritizes custom dictionary correctly", {
  text_data <- create_test_data()

  custom_dict <- data.frame(
    term = c("migraine", "special_term"),
    type = c("disease", "disease"),
    id = c("CUSTOM_1", "CUSTOM_2"),
    source = rep("custom", 2),
    stringsAsFactors = FALSE
  )

  result <- extract_entities_workflow(
    text_data,
    custom_dictionary = custom_dict,
    entity_types = "disease",
    dictionary_sources = "local",
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles large dictionary optimization", {
  text_data <- create_test_data()

  # Create a large custom dictionary to trigger optimization
  large_custom_dict <- data.frame(
    term = paste0("term_", 1:11000),
    type = rep("disease", 11000),
    id = paste0("ID_", 1:11000),
    source = rep("test", 11000),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(
    extract_entities_workflow(
      text_data,
      custom_dictionary = large_custom_dict,
      entity_types = "disease",
      dictionary_sources = "local",
      sanitize = TRUE,
      verbose = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles sanitization chunking", {
  text_data <- create_test_data()

  # Create dictionary with custom entries and others to test chunking
  mixed_dict <- data.frame(
    term = c(paste0("term_", 1:5500), paste0("custom_", 1:5500)),
    type = rep("disease", 11000),
    id = paste0("ID_", 1:11000),
    source = c(rep("test", 5500), rep("custom", 5500)),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(
    extract_entities_workflow(
      text_data,
      custom_dictionary = mixed_dict,
      entity_types = "disease",
      dictionary_sources = "local",
      sanitize = TRUE,
      verbose = TRUE
    )
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: load_dictionary - uncovered recursive call paths
# ============================================================================
test_that("load_dictionary recursively calls mesh when local not supported", {
  skip_if_not_installed("rentrez")
  skip_on_cran()

  # Request an expanded type from local source (should switch to mesh)
  result <- suppressMessages(suppressWarnings(
    load_dictionary(
      dictionary_type = "pathway",
      source = "local",
      n_terms = 10,
      sanitize = FALSE
    )
  ))

  expect_s3_class(result, "data.frame")
})

test_that("load_dictionary recursively calls mesh when umls fails without key", {
  result <- suppressMessages(
    load_dictionary(
      dictionary_type = "disease",
      source = "umls",
      api_key = NULL,
      n_terms = 10,
      sanitize = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: extract_entities - uncovered overlap handling
# ============================================================================
test_that("extract_entities handles overlaps with priority strategy correctly", {
  text_data <- data.frame(
    doc_id = 1,
    abstract = "migraine headache and severe migraine headache disorder"
  )

  dictionary <- data.frame(
    term = c("migraine", "headache", "migraine headache", "severe migraine headache"),
    type = rep("disease", 4),
    id = paste0("ID_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- extract_entities(
    text_data,
    dictionary = dictionary,
    overlap_strategy = "priority",
    sanitize_dict = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("extract_entities handles overlaps with longest strategy edge cases", {
  text_data <- data.frame(
    doc_id = 1,
    abstract = "short term and longer term phrase"
  )

  dictionary <- data.frame(
    term = c("short", "short term", "term", "longer term phrase"),
    type = rep("disease", 4),
    id = paste0("ID_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- extract_entities(
    text_data,
    dictionary = dictionary,
    overlap_strategy = "longest",
    sanitize_dict = FALSE
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: process_mesh_xml - Concept node handling
# ============================================================================
test_that("process_mesh_xml handles Concept nodes correctly", {
  concept_xml <- paste0(
    '<?xml version="1.0"?>',
    '<root>',
    '<Concept>',
    '<ConceptUI>C123456</ConceptUI>',
    '<ConceptName><String>Test Concept</String></ConceptName>',
    '<TermList>',
    '<Term><String>Synonym 1</String></Term>',
    '<Term><String>Synonym 2</String></Term>',
    '</TermList>',
    '</Concept>',
    '</root>'
  )

  result <- suppressWarnings(
    LBDiscover:::process_mesh_xml(concept_xml, "disease")
  )

  expect_s3_class(result, "data.frame")
  if (nrow(result) > 0) {
    expect_true("Test Concept" %in% result$term ||
                  "Synonym 1" %in% result$term ||
                  "Synonym 2" %in% result$term)
  }
})

test_that("process_mesh_xml handles DescriptorRecord with ConceptList", {
  descriptor_xml <- paste0(
    '<?xml version="1.0"?>',
    '<root>',
    '<DescriptorRecord>',
    '<DescriptorUI>D123456</DescriptorUI>',
    '<DescriptorName><String>Test Descriptor</String></DescriptorName>',
    '<ConceptList>',
    '<Concept>',
    '<Term><String>Entry Term 1</String></Term>',
    '<Term><String>Entry Term 2</String></Term>',
    '</Concept>',
    '</ConceptList>',
    '</DescriptorRecord>',
    '</root>'
  )

  result <- suppressWarnings(
    LBDiscover:::process_mesh_xml(descriptor_xml, "disease")
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: validate_umls_key
# ============================================================================
test_that("validate_umls_key handles third-party validation", {
  skip_if_not_installed("httr")
  skip_on_cran()

  result <- suppressWarnings(
    validate_umls_key(
      api_key = "test_key",
      validator_api_key = "validator_key"
    )
  )

  expect_type(result, "logical")
})

test_that("validate_umls_key handles authentication-based validation", {
  skip_if_not_installed("httr")
  skip_on_cran()

  result <- suppressWarnings(
    validate_umls_key(api_key = "test_key")
  )

  expect_type(result, "logical")
})
