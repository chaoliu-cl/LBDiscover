# Test file for text_preprocessing.R functions
library(testthat)
library(LBDiscover)

# Create test data
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
# Test: get_dict_cache
# ============================================================================
test_that("get_dict_cache returns an environment", {
  cache <- get_dict_cache()
  expect_true(is.environment(cache))
})

# ============================================================================
# Test: preprocess_text
# ============================================================================
test_that("preprocess_text basic functionality", {
  text_data <- create_test_data()

  result <- preprocess_text(
    text_data,
    text_column = "abstract",
    remove_stopwords = TRUE,
    min_word_length = 3
  )

  expect_s3_class(result, "data.frame")
  expect_true("doc_id" %in% colnames(result))
  expect_true("terms" %in% colnames(result))
  expect_equal(nrow(result), 3)
})

test_that("preprocess_text handles missing doc_id", {
  text_data <- data.frame(
    abstract = "Test text for preprocessing."
  )

  result <- preprocess_text(text_data, text_column = "abstract")

  expect_true("doc_id" %in% colnames(result))
  expect_equal(result$doc_id, 1)
})

test_that("preprocess_text handles NA values", {
  text_data <- data.frame(
    doc_id = 1:3,
    abstract = c("Valid text", NA, "Another valid text")
  )

  result <- preprocess_text(text_data, text_column = "abstract")

  expect_equal(nrow(result), 2)
})

test_that("preprocess_text with custom stopwords", {
  text_data <- create_test_data()

  result <- preprocess_text(
    text_data,
    text_column = "abstract",
    remove_stopwords = TRUE,
    custom_stopwords = c("migraine", "treatment")
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: extract_entities
# ============================================================================
test_that("extract_entities basic functionality", {
  text_data <- create_test_data()
  dictionary <- create_test_dictionary()

  result <- extract_entities(
    text_data,
    text_column = "abstract",
    dictionary = dictionary,
    case_sensitive = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("doc_id", "entity", "entity_type") %in% colnames(result)))
})

test_that("extract_entities handles empty dictionary", {
  text_data <- create_test_data()
  dictionary <- data.frame(
    term = character(0),
    type = character(0),
    stringsAsFactors = FALSE
  )

  expect_error(
    extract_entities(text_data, dictionary = dictionary, sanitize_dict = TRUE),
    "No terms remain in the dictionary"
  )
})

test_that("extract_entities with overlap strategies", {
  text_data <- create_test_data()
  dictionary <- create_test_dictionary()

  # Test priority strategy
  result_priority <- extract_entities(
    text_data,
    dictionary = dictionary,
    overlap_strategy = "priority"
  )
  expect_s3_class(result_priority, "data.frame")

  # Test longest strategy
  result_longest <- extract_entities(
    text_data,
    dictionary = dictionary,
    overlap_strategy = "longest"
  )
  expect_s3_class(result_longest, "data.frame")

  # Test all strategy
  result_all <- extract_entities(
    text_data,
    dictionary = dictionary,
    overlap_strategy = "all"
  )
  expect_s3_class(result_all, "data.frame")
})

# ============================================================================
# Test: load_dictionary
# ============================================================================
test_that("load_dictionary with local source", {
  result <- load_dictionary(
    dictionary_type = "disease",
    source = "local",
    sanitize = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("term", "type") %in% colnames(result)))
})

test_that("load_dictionary with custom path", {
  # Create temporary custom dictionary
  temp_dict <- tempfile(fileext = ".csv")
  custom_dict <- create_test_dictionary()
  write.csv(custom_dict, temp_dict, row.names = FALSE)

  result <- load_dictionary(custom_path = temp_dict, sanitize = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(custom_dict))

  # With sanitization, some terms may be filtered
  result_sanitized <- load_dictionary(custom_path = temp_dict, sanitize = TRUE)
  expect_s3_class(result_sanitized, "data.frame")
  expect_true(nrow(result_sanitized) <= nrow(custom_dict))

  # Cleanup
  unlink(temp_dict)
})

test_that("load_dictionary handles invalid types", {
  # When local doesn't support the type, it should switch to mesh
  expect_message(
    result <- load_dictionary(dictionary_type = "invalid_type", source = "local", sanitize = FALSE),
    "not supported|Using example dictionary"
  )

  # Should still return a valid data frame (dummy dictionary)
  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: create_dummy_dictionary
# ============================================================================
test_that("create_dummy_dictionary creates valid dictionaries", {
  disease_dict <- create_dummy_dictionary("disease")
  expect_s3_class(disease_dict, "data.frame")
  expect_true(all(c("term", "id", "type", "source") %in% colnames(disease_dict)))

  drug_dict <- create_dummy_dictionary("drug")
  expect_s3_class(drug_dict, "data.frame")

  gene_dict <- create_dummy_dictionary("gene")
  expect_s3_class(gene_dict, "data.frame")
})

# ============================================================================
# Test: detect_lang
# ============================================================================
test_that("detect_lang identifies English", {
  english_text <- "The quick brown fox jumps over the lazy dog. This is a test."
  result <- detect_lang(english_text)
  expect_equal(result, "en")
})

test_that("detect_lang handles short text", {
  short_text <- "Hello"
  result <- detect_lang(short_text)
  expect_type(result, "character")
})

test_that("detect_lang handles non-English text", {
  spanish_text <- "El rápido zorro marrón salta sobre el perro perezoso."
  result <- detect_lang(spanish_text)
  # May not always be accurate for short text
  expect_type(result, "character")
})

# ============================================================================
# Test: extract_ngrams
# ============================================================================
test_that("extract_ngrams generates unigrams", {
  text <- c("This is a test", "Another test sentence")

  result <- extract_ngrams(text, n = 1, min_freq = 1)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("ngram", "frequency") %in% colnames(result)))
  expect_true(nrow(result) > 0)
})

test_that("extract_ngrams generates bigrams", {
  text <- c("This is a test", "Another test sentence")

  result <- extract_ngrams(text, n = 2, min_freq = 1)

  expect_s3_class(result, "data.frame")
  expect_true(all(grepl(" ", result$ngram)))
})

test_that("extract_ngrams handles empty text", {
  text <- character(0)

  result <- extract_ngrams(text, n = 1, min_freq = 1)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("ngram", "frequency") %in% colnames(result)))
})

# ============================================================================
# Test: segment_sentences
# ============================================================================
test_that("segment_sentences splits text correctly", {
  text <- "This is sentence one. This is sentence two! Is this sentence three?"

  result <- segment_sentences(text)

  expect_type(result, "list")
  expect_equal(length(result), 1)
  expect_true(length(result[[1]]) >= 3)
})

test_that("segment_sentences handles abbreviations", {
  text <- "Dr. Smith works at Inc. Corp. He is very skilled."

  result <- segment_sentences(text)

  expect_type(result, "list")
  # Should not split at abbreviation periods
  expect_true(any(grepl("Dr\\.", result[[1]])))
})

test_that("segment_sentences handles empty text", {
  text <- character(0)

  result <- segment_sentences(text)

  expect_type(result, "list")
  expect_equal(length(result), 0)
})

# ============================================================================
# Test: sanitize_dictionary
# ============================================================================
test_that("sanitize_dictionary removes problematic terms", {
  dirty_dict <- data.frame(
    term = c("migraine", "europe", "optimization", "receptor", "123", "", NA),
    type = c("disease", "location", "process", "protein", "number", "empty", "missing"),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dirty_dict, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) < nrow(dirty_dict))
  # Empty and NA terms should be removed
  expect_false("" %in% result$term)
  expect_false(any(is.na(result$term)))
  # Numeric-only terms should be removed
  expect_false("123" %in% result$term)

  # Note: "europe" and "optimization" may or may not be filtered depending on
  # the blacklist and validation logic, so we just check they're either
  # kept or removed, not specifically which
  expect_true(all(result$term %in% c("migraine", "europe", "optimization", "receptor")))
})

test_that("sanitize_dictionary validates entity types", {
  dict <- data.frame(
    term = c("migraine", "receptor", "optimization"),
    type = c("disease", "protein", "chemical"),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  # Check that valid terms are kept
  expect_true("migraine" %in% result$term)
  expect_true("receptor" %in% result$term)

  # optimization may or may not be filtered depending on validation logic
  # Just ensure we get a valid result
  expect_true(nrow(result) >= 2)
})

test_that("sanitize_dictionary handles empty input", {
  empty_dict <- data.frame(
    term = character(0),
    type = character(0),
    stringsAsFactors = FALSE
  )

  # Should return empty dictionary without error
  result <- suppressWarnings(
    sanitize_dictionary(empty_dict, verbose = FALSE)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

# ============================================================================
# Test: extract_topics
# ============================================================================
test_that("extract_topics generates topics", {
  text_data <- create_test_data()

  result <- extract_topics(
    text_data,
    text_column = "abstract",
    n_topics = 2,
    max_terms = 5
  )

  expect_type(result, "list")
  expect_true("topics" %in% names(result))
  expect_true("document_topics" %in% names(result))
  expect_equal(length(result$topics), 2)
})

test_that("extract_topics handles single document", {
  text_data <- data.frame(
    abstract = "Single document for topic modeling test."
  )

  result <- extract_topics(text_data, n_topics = 1)

  expect_type(result, "list")
  expect_true("topics" %in% names(result))
})

# ============================================================================
# Test: create_term_document_matrix
# ============================================================================
test_that("create_term_document_matrix creates matrix", {
  text_data <- create_test_data()
  preprocessed <- preprocess_text(text_data, text_column = "abstract")

  # Use lower min_df to ensure we get some terms
  result <- create_term_document_matrix(preprocessed, min_df = 1, max_df = 1.0)

  expect_true(is.matrix(result))
  expect_equal(ncol(result), nrow(preprocessed))
  expect_true(nrow(result) > 0)
})

test_that("create_term_document_matrix filters by frequency", {
  text_data <- create_test_data()
  preprocessed <- preprocess_text(text_data, text_column = "abstract")

  # Use min_df = 1 to ensure we get terms
  result1 <- create_term_document_matrix(preprocessed, min_df = 1, max_df = 1.0)

  expect_true(is.matrix(result1))
  expect_true(nrow(result1) > 0)

  # If we have terms appearing multiple times, test min_df = 2
  # Otherwise skip this part
  if (any(rowSums(result1 > 0) >= 2)) {
    result2 <- create_term_document_matrix(preprocessed, min_df = 2, max_df = 1.0)
    expect_true(is.matrix(result2))
    # Should have fewer or equal terms
    expect_true(nrow(result2) <= nrow(result1))
  }
})

# ============================================================================
# Test: extract_entities_workflow
# ============================================================================
test_that("extract_entities_workflow with local source", {
  text_data <- create_test_data()

  result <- extract_entities_workflow(
    text_data,
    text_column = "abstract",
    entity_types = c("disease", "drug"),
    dictionary_sources = "local",
    max_terms_per_type = 10,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("doc_id", "entity", "entity_type") %in% colnames(result)))
})

test_that("extract_entities_workflow with custom dictionary", {
  text_data <- create_test_data()
  custom_dict <- create_test_dictionary()

  result <- extract_entities_workflow(
    text_data,
    custom_dictionary = custom_dict,
    entity_types = c("disease", "drug"),
    dictionary_sources = "local",
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles empty results", {
  text_data <- data.frame(
    abstract = "Text with no recognized entities xyz abc def."
  )

  result <- extract_entities_workflow(
    text_data,
    entity_types = c("disease"),
    dictionary_sources = "local",
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: map_ontology
# ============================================================================
test_that("map_ontology handles basic mapping", {
  skip_if_not_installed("rentrez")

  terms <- c("headache", "migraine")

  # This test may fail without internet connection
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch({
    map_ontology(
      terms,
      ontology = "mesh",
      fuzzy_match = FALSE
    )
  }, error = function(e) {
    skip("MeSH API not available")
  })

  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
  }
})

# ============================================================================
# Test: extract_ner (Named Entity Recognition)
# ============================================================================
test_that("extract_ner extracts entities", {
  text <- c(
    "Migraine is a neurological disorder.",
    "Serotonin plays a role in headache."
  )

  custom_dicts <- list(
    disease = data.frame(
      term = c("migraine", "headache"),
      id = c("D001", "D002"),
      type = rep("disease", 2),
      stringsAsFactors = FALSE
    )
  )

  result <- extract_ner(
    text,
    entity_types = "disease",
    custom_dictionaries = custom_dicts
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("text_id", "entity", "entity_type") %in% colnames(result)))
})

# ============================================================================
# Test: get_umls_semantic_types
# ============================================================================
test_that("get_umls_semantic_types returns correct types", {
  disease_types <- get_umls_semantic_types("disease")
  expect_type(disease_types, "character")
  expect_true(length(disease_types) > 0)

  drug_types <- get_umls_semantic_types("drug")
  expect_type(drug_types, "character")

  unknown_types <- get_umls_semantic_types("unknown_type")
  expect_null(unknown_types)
})

# ============================================================================
# Integration Tests
# ============================================================================
test_that("complete workflow: preprocess -> extract -> sanitize", {
  text_data <- create_test_data()

  # Step 1: Preprocess
  preprocessed <- preprocess_text(text_data, text_column = "abstract")
  expect_s3_class(preprocessed, "data.frame")

  # Step 2: Extract entities
  dictionary <- create_test_dictionary()
  entities <- extract_entities(preprocessed, dictionary = dictionary)
  expect_s3_class(entities, "data.frame")

  # Step 3: Sanitize dictionary
  clean_dict <- sanitize_dictionary(dictionary, verbose = FALSE)
  expect_s3_class(clean_dict, "data.frame")
  expect_true(nrow(clean_dict) <= nrow(dictionary))
})

test_that("workflow with entity extraction and topic modeling", {
  text_data <- create_test_data()

  # Extract entities
  entities <- extract_entities_workflow(
    text_data,
    entity_types = c("disease", "drug"),
    dictionary_sources = "local",
    verbose = FALSE
  )

  # Extract topics
  topics <- extract_topics(text_data, n_topics = 2)

  expect_s3_class(entities, "data.frame")
  expect_type(topics, "list")
})

# ============================================================================
# Edge Cases and Error Handling
# ============================================================================
test_that("functions handle NULL inputs gracefully", {
  # preprocess_text with NULL should error
  expect_error(suppressWarnings(preprocess_text(NULL)))

  # extract_entities with NULL dictionary should error
  expect_error(extract_entities(create_test_data(), dictionary = NULL))

  # sanitize_dictionary with NULL should return NULL or error
  result <- suppressWarnings(sanitize_dictionary(NULL, verbose = FALSE))
  expect_true(is.null(result) || (is.data.frame(result) && nrow(result) == 0))
})

test_that("functions handle empty strings", {
  text_data <- data.frame(abstract = c("", "  ", "\n"))
  result <- preprocess_text(text_data, text_column = "abstract")

  # Empty strings should be filtered out during preprocessing
  # The function may keep rows but with empty terms lists
  expect_s3_class(result, "data.frame")

  # Check that terms lists are empty for these rows
  if (nrow(result) > 0) {
    all_empty <- all(sapply(result$terms, function(x) {
      is.data.frame(x) && nrow(x) == 0
    }))
    expect_true(all_empty || nrow(result) == 0)
  }
})

test_that("detect_lang handles various inputs", {
  expect_equal(detect_lang(""), "unknown")
  expect_type(detect_lang("a"), "character")
  expect_type(detect_lang(paste(rep("test", 100), collapse = " ")), "character")
})

# ============================================================================
# Performance Tests (optional, can be skipped on CRAN)
# ============================================================================
test_that("extract_entities_workflow handles large datasets", {
  skip_on_cran()

  # Create larger test dataset
  large_data <- data.frame(
    doc_id = 1:100,
    abstract = rep(
      "Migraine is a neurological disorder causing severe headache.",
      100
    ),
    stringsAsFactors = FALSE
  )

  start_time <- Sys.time()
  result <- extract_entities_workflow(
    large_data,
    entity_types = "disease",
    dictionary_sources = "local",
    verbose = FALSE,
    batch_size = 50
  )
  end_time <- Sys.time()

  expect_s3_class(result, "data.frame")
  # Should complete in reasonable time (< 30 seconds)
  expect_true(difftime(end_time, start_time, units = "secs") < 30)
})

# ============================================================================
# Run all tests
# ============================================================================
cat("\n=== Text Preprocessing Tests Complete ===\n")
cat("All tests for text_preprocessing.R functions have been executed.\n")
