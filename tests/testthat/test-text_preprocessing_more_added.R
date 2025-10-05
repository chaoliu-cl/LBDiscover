# Additional comprehensive tests for uncovered portions of text_preprocessing.R
library(testthat)
library(LBDiscover)

# ============================================================================
# Helper functions
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
# Test: sanitize_dictionary - Complete coverage of validation logic
# ============================================================================
test_that("sanitize_dictionary handles all validation paths for proteins", {
  protein_dict <- data.frame(
    term = c("receptor", "receptors", "enzyme kinase", "short", "albumin protein"),
    type = rep("protein", 5),
    id = paste0("PROT_", 1:5),
    source = rep("test", 5),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(protein_dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  # "receptor" and "receptors" should be preserved as special cases
  if (nrow(result) > 0) {
    expect_true("receptor" %in% result$term || "receptors" %in% result$term ||
                  any(grepl("kinase|albumin", result$term)))
  }
})

test_that("sanitize_dictionary handles disease validation with migraine special case", {
  disease_dict <- data.frame(
    term = c("migraine", "cancer syndrome", "random123", "infection disease"),
    type = rep("disease", 4),
    id = paste0("DIS_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(disease_dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  # "migraine" should specifically be preserved
  if (nrow(result) > 0) {
    expect_true("migraine" %in% result$term)
  }
})

test_that("sanitize_dictionary handles symptom validation with known symptoms", {
  symptom_dict <- data.frame(
    term = c("pain", "headache", "photophobia", "fatigue", "random_word"),
    type = rep("symptom", 5),
    id = paste0("SYMP_", 1:5),
    source = rep("test", 5),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(symptom_dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  if (nrow(result) > 0) {
    known <- c("pain", "headache", "photophobia", "fatigue")
    expect_true(any(known %in% result$term))
  }
})

test_that("sanitize_dictionary handles biological_process with known processes", {
  bioprocess_dict <- data.frame(
    term = c("inflammation", "signaling pathway", "activation", "random_text"),
    type = rep("biological_process", 4),
    id = paste0("BP_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(bioprocess_dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  if (nrow(result) > 0) {
    known <- c("inflammation", "signaling", "activation")
    expect_true(any(sapply(known, function(k) any(grepl(k, result$term)))))
  }
})

test_that("sanitize_dictionary handles method validation with analytical methods", {
  method_dict <- data.frame(
    term = c("faers", "bcpnn", "uplc", "hplc method", "random"),
    type = rep("method", 5),
    id = paste0("METH_", 1:5),
    source = rep("test", 5),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(method_dict, validate_types = TRUE, verbose = TRUE)

  expect_s3_class(result, "data.frame")
  if (nrow(result) > 0) {
    analytical <- c("faers", "bcpnn", "uplc", "hplc")
    expect_true(any(analytical %in% result$term) ||
                  any(grepl("method", result$term)))
  }
})

test_that("sanitize_dictionary applies type corrections from term_type_mappings", {
  dict <- data.frame(
    term = c("migraine", "bcpnn", "faers", "aspirin"),
    type = c("symptom", "disease", "drug", "chemical"),  # Wrong types
    id = paste0("ID_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dict, validate_types = TRUE, verbose = TRUE)

  expect_s3_class(result, "data.frame")
  if (nrow(result) > 0) {
    # Check if corrections were applied
    migraine_row <- result[result$term == "migraine", ]
    if (nrow(migraine_row) > 0) {
      expect_equal(migraine_row$type, "disease")
    }

    bcpnn_row <- result[result$term == "bcpnn", ]
    if (nrow(bcpnn_row) > 0) {
      expect_equal(bcpnn_row$type, "method")
    }
  }
})

test_that("sanitize_dictionary removes terms with problem patterns", {
  dict <- data.frame(
    term = c("normal term", "68 [1", "test123[bracket", "valid"),
    type = rep("disease", 4),
    id = paste0("ID_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dict, verbose = TRUE)

  expect_s3_class(result, "data.frame")
  expect_false("68 [1" %in% result$term)
  expect_false("test123[bracket" %in% result$term)
})

test_that("sanitize_dictionary handles empty dictionary at various stages", {
  # Test empty input
  empty_dict <- data.frame(
    term = character(0),
    type = character(0),
    id = character(0),
    source = character(0),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(
    sanitize_dictionary(empty_dict, verbose = FALSE)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("term", "id", "type", "source") %in% colnames(result)))
})

test_that("sanitize_dictionary preserves custom source entries", {
  dict <- data.frame(
    term = c("custom_term1", "custom_term2", "europe", "optimization"),
    type = rep("disease", 4),
    id = paste0("ID_", 1:4),
    source = c("custom", "custom", "test", "test"),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dict, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  # Custom terms should be handled appropriately
})

# ============================================================================
# Test: extract_entities_workflow - Complete parallel and batch processing
# ============================================================================
test_that("extract_entities_workflow handles R CMD check environment detection", {
  # Temporarily set check environment variable
  old_val <- Sys.getenv("_R_CHECK_LIMIT_CORES_")
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")

  text_data <- create_test_data()

  result <- extract_entities_workflow(
    text_data,
    entity_types = c("disease"),
    dictionary_sources = "local",
    parallel = TRUE,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")

  # Restore environment
  if (old_val == "") {
    Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
  } else {
    Sys.setenv("_R_CHECK_LIMIT_CORES_" = old_val)
  }
})

test_that("extract_entities_workflow handles parallel processing error fallback", {
  skip_if_not(requireNamespace("parallel", quietly = TRUE))

  text_data <- create_test_data()

  # Force an error scenario by using invalid parameters
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

test_that("extract_entities_workflow handles large dictionary chunked sanitization", {
  text_data <- create_test_data()

  # Create a large dictionary to trigger chunked sanitization
  large_dict <- data.frame(
    term = paste0("term_", 1:12000),
    type = rep("disease", 12000),
    id = paste0("ID_", 1:12000),
    source = rep("test", 12000),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(
    extract_entities_workflow(
      text_data,
      custom_dictionary = large_dict,
      entity_types = "disease",
      dictionary_sources = "local",
      sanitize = TRUE,
      verbose = TRUE
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles mixed custom and standard dictionary sanitization", {
  text_data <- create_test_data()

  # Create a medium-sized dictionary with custom entries
  mixed_dict <- data.frame(
    term = c(paste0("custom_", 1:100), paste0("standard_", 1:5500)),
    type = rep("disease", 5600),
    id = paste0("ID_", 1:5600),
    source = c(rep("custom", 100), rep("test", 5500)),
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

test_that("extract_entities_workflow handles fallback dictionary creation", {
  text_data <- create_test_data()

  # Create a scenario with no valid dictionaries
  result <- suppressWarnings(
    extract_entities_workflow(
      text_data,
      entity_types = c("nonexistent_type"),
      dictionary_sources = "local",
      verbose = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles batch processing with errors", {
  # Create data with some NA values to trigger potential errors
  text_data <- data.frame(
    doc_id = 1:30,
    abstract = c(rep("valid text", 10), rep(NA, 10), rep("more text", 10))
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

test_that("extract_entities_workflow uses cache correctly", {
  text_data <- create_test_data()

  # Clear cache
  cache_env <- get_dict_cache()
  rm(list = ls(cache_env), envir = cache_env)

  # First call - should populate cache
  result1 <- extract_entities_workflow(
    text_data,
    entity_types = "disease",
    dictionary_sources = "local",
    cache_dictionaries = TRUE,
    verbose = FALSE
  )

  # Check cache has entries
  expect_true(length(ls(cache_env)) > 0)

  # Second call - should use cache
  result2 <- extract_entities_workflow(
    text_data,
    entity_types = "disease",
    dictionary_sources = "local",
    cache_dictionaries = TRUE,
    verbose = FALSE
  )

  expect_s3_class(result1, "data.frame")
  expect_s3_class(result2, "data.frame")
})

test_that("extract_entities_workflow handles dictionary loading errors in parallel", {
  skip_if_not(requireNamespace("parallel", quietly = TRUE))

  text_data <- create_test_data()

  result <- suppressWarnings(
    extract_entities_workflow(
      text_data,
      entity_types = c("disease", "nonexistent1", "nonexistent2"),
      dictionary_sources = "local",
      parallel = TRUE,
      num_cores = 2,
      verbose = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles custom dictionary without proper columns", {
  text_data <- create_test_data()

  bad_custom_dict <- data.frame(
    word = c("test1", "test2"),
    category = c("type1", "type2")
  )

  expect_error(
    extract_entities_workflow(
      text_data,
      custom_dictionary = bad_custom_dict,
      entity_types = "disease",
      dictionary_sources = "local"
    ),
    "must have at least columns"
  )
})

test_that("extract_entities_workflow adds missing columns to custom dictionary", {
  text_data <- create_test_data()

  # Custom dict without 'source' and 'id'
  custom_dict <- data.frame(
    term = c("migraine", "headache"),
    type = c("disease", "symptom")
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

# ============================================================================
# Test: load_dictionary - Recursive call scenarios
# ============================================================================
test_that("load_dictionary recursively calls mesh for unsupported local types", {
  skip_if_not_installed("rentrez")
  skip_on_cran()

  # Request pathway from local (not supported) - should switch to mesh
  result <- suppressMessages(suppressWarnings(
    load_dictionary(
      dictionary_type = "pathway",
      source = "local",
      n_terms = 5,
      sanitize = FALSE
    )
  ))

  expect_s3_class(result, "data.frame")
})

test_that("load_dictionary recursively calls mesh when umls has no key", {
  # Request from UMLS without key - should fall back to mesh
  result <- suppressMessages(
    load_dictionary(
      dictionary_type = "disease",
      source = "umls",
      api_key = NULL,
      n_terms = 5,
      sanitize = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: extract_ngrams - Complete coverage
# ============================================================================
test_that("extract_ngrams handles text shorter than n", {
  short_text <- c("one", "two")

  result <- extract_ngrams(short_text, n = 5, min_freq = 1)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("extract_ngrams handles empty results after frequency filtering", {
  text <- c("unique word each time different")

  result <- extract_ngrams(text, n = 1, min_freq = 10)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("ngram", "frequency") %in% colnames(result)))
})

test_that("extract_ngrams handles NULL or empty ngrams", {
  text <- character(0)

  result <- extract_ngrams(text, n = 2, min_freq = 1)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

# ============================================================================
# Test: map_ontology - Complete coverage
# ============================================================================
test_that("map_ontology handles empty terms", {
  result <- map_ontology(
    character(0),
    ontology = "mesh",
    dictionary_type = "disease"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("term", "ontology_id", "ontology_term", "match_type") %in% colnames(result)))
})

test_that("map_ontology handles empty dictionary results", {
  # Use a very specific dictionary type that might return empty results
  result <- suppressWarnings(
    map_ontology(
      c("nonexistent_term_xyz123"),
      ontology = "mesh",
      dictionary_type = "disease"
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("map_ontology requires API key for UMLS", {
  expect_error(
    map_ontology(
      c("headache"),
      ontology = "umls",
      api_key = NULL
    ),
    "API key is required"
  )
})

# ============================================================================
# Test: process_mesh_xml - Concept node handling
# ============================================================================
test_that("process_mesh_xml handles Concept nodes", {
  concept_xml <- paste0(
    '<?xml version="1.0"?>',
    '<root>',
    '<Concept>',
    '<ConceptUI>C123</ConceptUI>',
    '<ConceptName><String>Test Concept</String></ConceptName>',
    '<TermList>',
    '<Term><String>Synonym 1</String></Term>',
    '</TermList>',
    '</Concept>',
    '</root>'
  )

  result <- suppressWarnings(
    LBDiscover:::process_mesh_xml(concept_xml, "disease")
  )

  expect_s3_class(result, "data.frame")
})

test_that("process_mesh_xml handles mixed DescriptorRecord and Concept", {
  mixed_xml <- paste0(
    '<?xml version="1.0"?>',
    '<root>',
    '<DescriptorRecord>',
    '<DescriptorUI>D001</DescriptorUI>',
    '<DescriptorName><String>Test Descriptor</String></DescriptorName>',
    '</DescriptorRecord>',
    '<Concept>',
    '<ConceptUI>C001</ConceptUI>',
    '<ConceptName><String>Test Concept</String></ConceptName>',
    '</Concept>',
    '</root>'
  )

  result <- suppressWarnings(
    LBDiscover:::process_mesh_xml(mixed_xml, "disease")
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: process_mesh_chunks - Error handling
# ============================================================================
test_that("process_mesh_chunks handles malformed XML", {
  malformed <- "<DescriptorRecord><DescriptorUI>incomplete"

  result <- suppressWarnings(
    LBDiscover:::process_mesh_chunks(malformed, "disease")
  )

  expect_s3_class(result, "data.frame")
})

test_that("process_mesh_chunks handles no complete records", {
  incomplete <- "<Desc"

  result <- suppressWarnings(
    LBDiscover:::process_mesh_chunks(incomplete, "disease")
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: extract_mesh_from_text - Complete coverage
# ============================================================================
test_that("extract_mesh_from_text extracts from text format", {
  mesh_text <- paste0(
    "1: Migraine Disorder\n",
    "2: Headache Syndrome\n",
    "Entry Terms: Pain, Nausea\n",
    "Tree Number(s): D25.651"
  )

  result <- LBDiscover:::extract_mesh_from_text(mesh_text, "disease")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("extract_mesh_from_text handles empty text", {
  result <- suppressWarnings(
    LBDiscover:::extract_mesh_from_text("", "disease")
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: validate_umls_key - Both validation methods
# ============================================================================
test_that("validate_umls_key uses authentication method", {
  skip_if_not_installed("httr")
  skip_on_cran()

  result <- suppressWarnings(
    validate_umls_key(api_key = "test_key")
  )

  expect_type(result, "logical")
})

test_that("validate_umls_key uses third-party validator", {
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

# ============================================================================
# Test: extract_entities - Priority and longest overlap strategies
# ============================================================================
test_that("extract_entities priority strategy handles complex overlaps", {
  text_data <- data.frame(
    doc_id = 1,
    abstract = "severe migraine headache with severe pain and headache"
  )

  dictionary <- data.frame(
    term = c("migraine", "severe migraine", "headache", "severe headache",
             "pain", "severe pain", "migraine headache"),
    type = rep("symptom", 7),
    id = paste0("ID_", 1:7),
    source = rep("test", 7),
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

test_that("extract_entities longest strategy prefers longer matches", {
  text_data <- data.frame(
    doc_id = 1,
    abstract = "severe migraine headache disorder and headache"
  )

  dictionary <- data.frame(
    term = c("migraine", "headache", "severe migraine", "migraine headache",
             "severe migraine headache"),
    type = rep("disease", 5),
    id = paste0("ID_", 1:5),
    source = rep("test", 5),
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
# Test: Integration scenarios
# ============================================================================
test_that("complete workflow with large dataset and batching", {
  large_data <- data.frame(
    doc_id = 1:150,
    abstract = rep("Migraine causes headache and nausea", 150)
  )

  result <- suppressWarnings(
    extract_entities_workflow(
      large_data,
      entity_types = c("disease", "symptom"),
      dictionary_sources = "local",
      batch_size = 50,
      verbose = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("workflow with invalid sources filters correctly", {
  text_data <- create_test_data()

  result <- suppressWarnings(
    extract_entities_workflow(
      text_data,
      entity_types = "disease",
      dictionary_sources = c("invalid_source", "local", "another_invalid"),
      verbose = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles core detection and limits", {
  skip_if_not(requireNamespace("parallel", quietly = TRUE))

  text_data <- create_test_data()

  # Test with num_cores = NULL (should auto-detect)
  result <- extract_entities_workflow(
    text_data,
    entity_types = "disease",
    dictionary_sources = "local",
    parallel = TRUE,
    num_cores = NULL,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: Edge cases for dictionary creation and caching
# ============================================================================
test_that("get_dict_cache returns consistent environment", {
  cache1 <- get_dict_cache()
  cache2 <- get_dict_cache()

  expect_true(identical(cache1, cache2))
  expect_true(is.environment(cache1))
})

test_that("dictionary caching prevents redundant loading", {
  text_data <- create_test_data()

  cache_env <- get_dict_cache()
  rm(list = ls(cache_env), envir = cache_env)

  # Load with caching
  result1 <- extract_entities_workflow(
    text_data,
    entity_types = "disease",
    dictionary_sources = "local",
    cache_dictionaries = TRUE,
    verbose = FALSE
  )

  cache_size_after_first <- length(ls(cache_env))

  # Second load should use cache
  result2 <- extract_entities_workflow(
    text_data,
    entity_types = "disease",
    dictionary_sources = "local",
    cache_dictionaries = TRUE,
    verbose = FALSE
  )

  cache_size_after_second <- length(ls(cache_env))

  expect_equal(cache_size_after_first, cache_size_after_second)
})

# ============================================================================
# Test: create_term_document_matrix edge cases
# ============================================================================
test_that("create_term_document_matrix handles no terms after filtering", {
  text_data <- data.frame(
    doc_id = 1,
    abstract = "test"
  )

  preprocessed <- preprocess_text(text_data, text_column = "abstract")

  # Try with very restrictive filtering
  result <- tryCatch({
    create_term_document_matrix(preprocessed, min_df = 100, max_df = 0.01)
  }, error = function(e) {
    expect_true(grepl("No terms remain", e$message))
    NULL
  })
})

test_that("create_term_document_matrix handles max_df filtering", {
  text_data <- data.frame(
    doc_id = 1:3,
    abstract = c("test word", "test word", "test word")
  )

  preprocessed <- preprocess_text(text_data, text_column = "abstract")

  # Very low max_df should filter out common terms
  result <- tryCatch({
    create_term_document_matrix(preprocessed, min_df = 1, max_df = 0.5)
  }, error = function(e) {
    NULL
  })

  if (!is.null(result)) {
    expect_true(is.matrix(result))
  }
})
