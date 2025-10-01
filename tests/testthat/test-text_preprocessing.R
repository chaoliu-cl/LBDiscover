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

test_that("preprocess_text with stemming", {
  skip_if_not_installed("SnowballC")

  text_data <- create_test_data()

  result <- preprocess_text(
    text_data,
    text_column = "abstract",
    stem_words = TRUE,
    remove_stopwords = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_true("terms" %in% colnames(result))
})

test_that("preprocess_text errors without SnowballC when stemming requested", {
  skip_if(requireNamespace("SnowballC", quietly = TRUE),
          "SnowballC is installed")

  text_data <- create_test_data()

  expect_error(
    preprocess_text(text_data, stem_words = TRUE),
    "SnowballC package is required"
  )
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
# Test: extract_entities - overlap strategies
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
    suppressWarnings(extract_entities(text_data, dictionary = dictionary, sanitize_dict = TRUE)),
    "No terms remain in the dictionary"
  )
})

test_that("extract_entities with overlap strategies - priority", {
  text_data <- data.frame(
    doc_id = 1,
    abstract = "severe headache pain and headache disorders"
  )

  dictionary <- data.frame(
    term = c("headache", "severe headache", "pain"),
    type = c("symptom", "symptom", "symptom"),
    id = paste0("SYM_", 1:3),
    source = rep("test", 3),
    stringsAsFactors = FALSE
  )

  result_priority <- extract_entities(
    text_data,
    dictionary = dictionary,
    overlap_strategy = "priority",
    sanitize_dict = FALSE
  )

  expect_s3_class(result_priority, "data.frame")
  expect_true(nrow(result_priority) > 0)
})

test_that("extract_entities with overlap strategies - longest", {
  text_data <- data.frame(
    doc_id = 1,
    abstract = "severe headache and migraine headache"
  )

  dictionary <- data.frame(
    term = c("headache", "severe headache", "migraine headache"),
    type = c("symptom", "symptom", "symptom"),
    id = paste0("SYM_", 1:3),
    source = rep("test", 3),
    stringsAsFactors = FALSE
  )

  result_longest <- extract_entities(
    text_data,
    dictionary = dictionary,
    overlap_strategy = "longest",
    sanitize_dict = FALSE
  )

  expect_s3_class(result_longest, "data.frame")
})

test_that("extract_entities with overlap strategies - all", {
  text_data <- create_test_data()
  dictionary <- create_test_dictionary()

  result_all <- extract_entities(
    text_data,
    dictionary = dictionary,
    overlap_strategy = "all",
    sanitize_dict = FALSE
  )

  expect_s3_class(result_all, "data.frame")
})

# ============================================================================
# Test: load_dictionary - various sources and error handling
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

test_that("load_dictionary with custom path - CSV", {
  temp_dict <- tempfile(fileext = ".csv")
  custom_dict <- create_test_dictionary()
  write.csv(custom_dict, temp_dict, row.names = FALSE)

  result <- load_dictionary(custom_path = temp_dict, sanitize = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(custom_dict))

  unlink(temp_dict)
})

test_that("load_dictionary with custom path - RDS", {
  temp_dict <- tempfile(fileext = ".rds")
  custom_dict <- create_test_dictionary()
  saveRDS(custom_dict, temp_dict)

  result <- load_dictionary(custom_path = temp_dict, sanitize = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(custom_dict))

  unlink(temp_dict)
})

test_that("load_dictionary with custom path - unsupported format", {
  temp_dict <- tempfile(fileext = ".txt")
  writeLines("test", temp_dict)

  expect_error(
    load_dictionary(custom_path = temp_dict),
    "Unsupported file format"
  )

  unlink(temp_dict)
})

test_that("load_dictionary with custom path - missing file", {
  expect_error(
    load_dictionary(custom_path = "nonexistent_file.csv"),
    "Custom dictionary file not found"
  )
})

test_that("load_dictionary with custom path - missing required columns", {
  temp_dict <- tempfile(fileext = ".csv")
  bad_dict <- data.frame(
    word = c("test1", "test2"),
    category = c("type1", "type2")
  )
  write.csv(bad_dict, temp_dict, row.names = FALSE)

  expect_error(
    load_dictionary(custom_path = temp_dict),
    "Dictionary must have columns"
  )

  unlink(temp_dict)
})

test_that("load_dictionary handles invalid types for local source", {
  suppressWarnings(
    expect_message(
      result <- load_dictionary(dictionary_type = "invalid_type", source = "local", sanitize = FALSE),
      "not supported|Using example dictionary"
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("load_dictionary switches to mesh for unsupported local types", {
  skip_if_not_installed("rentrez")
  skip_on_cran()

  suppressWarnings(
    expect_message(
      result <- load_dictionary(dictionary_type = "protein", source = "local", sanitize = FALSE),
      "not supported"
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("load_dictionary with UMLS requires API key", {
  expect_message(
    result <- load_dictionary(dictionary_type = "disease", source = "umls", api_key = NULL, sanitize = FALSE),
    "API key is required"
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: load_from_mesh (internal function testing)
# ============================================================================
test_that("load_from_mesh returns dummy dictionary when rentrez not available", {
  skip_if(requireNamespace("rentrez", quietly = TRUE), "rentrez is available")

  result <- LBDiscover:::load_from_mesh("disease", n_terms = 10)

  expect_s3_class(result, "data.frame")
  expect_true("source" %in% colnames(result))
})

test_that("load_from_mesh returns dummy dictionary when xml2 not available", {
  skip_if(requireNamespace("xml2", quietly = TRUE), "xml2 is available")
  skip_if_not_installed("rentrez")

  result <- LBDiscover:::load_from_mesh("disease", n_terms = 10)

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

  protein_dict <- create_dummy_dictionary("protein")
  expect_s3_class(protein_dict, "data.frame")

  chemical_dict <- create_dummy_dictionary("chemical")
  expect_s3_class(chemical_dict, "data.frame")

  pathway_dict <- create_dummy_dictionary("pathway")
  expect_s3_class(pathway_dict, "data.frame")

  symptom_dict <- create_dummy_dictionary("symptom")
  expect_s3_class(symptom_dict, "data.frame")

  anatomy_dict <- create_dummy_dictionary("anatomy")
  expect_s3_class(anatomy_dict, "data.frame")
})

test_that("create_dummy_dictionary handles unknown types", {
  result <- create_dummy_dictionary("unknown_type")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
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

test_that("detect_lang handles empty/NULL text", {
  expect_equal(detect_lang(""), "unknown")
  expect_equal(detect_lang(NULL), "unknown")
  expect_equal(detect_lang(character(0)), "unknown")
})

test_that("detect_lang handles non-English text", {
  spanish_text <- "El rápido zorro marrón salta sobre el perro perezoso."
  result <- detect_lang(spanish_text)
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

test_that("extract_ngrams handles NA values", {
  text <- c("test", NA, "another test")

  result <- extract_ngrams(text, n = 1, min_freq = 1)

  expect_s3_class(result, "data.frame")
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
  expect_true(any(grepl("Dr\\.", result[[1]])))
})

test_that("segment_sentences handles empty text", {
  text <- character(0)

  result <- segment_sentences(text)

  expect_type(result, "list")
  expect_equal(length(result), 0)
})

# ============================================================================
# Test: sanitize_dictionary - comprehensive
# ============================================================================
test_that("sanitize_dictionary removes problematic terms", {
  dirty_dict <- data.frame(
    term = c("migraine", "europe", "optimization", "receptor", "123", "", NA),
    type = c("disease", "location", "process", "protein", "number", "empty", "missing"),
    id = paste0("ID_", 1:7),
    source = rep("test", 7),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dirty_dict, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) < nrow(dirty_dict))
  expect_false("" %in% result$term)
  expect_false(any(is.na(result$term)))
  expect_false("123" %in% result$term)
})

test_that("sanitize_dictionary removes terms with regex special characters", {
  dict <- data.frame(
    term = c("normal", "with[bracket]", "with(paren)", "with{brace}"),
    type = rep("disease", 4),
    id = paste0("ID_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dict, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_false("with[bracket]" %in% result$term)
  expect_false("with(paren)" %in% result$term)
  expect_false("with{brace}" %in% result$term)
  # Note: "normal" may be filtered out if it's in the blacklist
  # Check if normal survived or was legitimately filtered
  if (nrow(result) > 0) {
    expect_true(all(!grepl("[\\[\\]\\(\\)\\{\\}]", result$term)))
  }
})

test_that("sanitize_dictionary removes terms with numbers followed by special characters", {
  dict <- data.frame(
    term = c("normal", "test123[", "68 [1", "valid123"),
    type = rep("disease", 4),
    id = paste0("ID_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dict, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_false("test123[" %in% result$term)
  expect_false("68 [1" %in% result$term)
})

test_that("sanitize_dictionary validates entity types", {
  dict <- data.frame(
    term = c("migraine", "receptor", "optimization"),
    type = c("disease", "protein", "chemical"),
    id = paste0("ID_", 1:3),
    source = rep("test", 3),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true("migraine" %in% result$term)
  expect_true("receptor" %in% result$term)
})

test_that("sanitize_dictionary applies type corrections", {
  dict <- data.frame(
    term = c("migraine", "headache", "serotonin", "receptor"),
    type = c("symptom", "disease", "protein", "gene"),  # Intentionally wrong
    id = paste0("ID_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(dict, validate_types = TRUE, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  # Check that corrections were applied
  migraine_row <- result[result$term == "migraine", ]
  if (nrow(migraine_row) > 0) {
    expect_equal(migraine_row$type, "disease")
  }
})

test_that("sanitize_dictionary handles empty input", {
  empty_dict <- data.frame(
    term = character(0),
    type = character(0),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(
    sanitize_dictionary(empty_dict, verbose = FALSE)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("sanitize_dictionary handles NULL input", {
  result <- suppressWarnings(
    sanitize_dictionary(NULL, verbose = FALSE)
  )

  expect_true(is.null(result) || (is.data.frame(result) && nrow(result) == 0))
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

  result <- create_term_document_matrix(preprocessed, min_df = 1, max_df = 1.0)

  expect_true(is.matrix(result))
  expect_equal(ncol(result), nrow(preprocessed))
  expect_true(nrow(result) > 0)
})

test_that("create_term_document_matrix filters by frequency", {
  text_data <- create_test_data()
  preprocessed <- preprocess_text(text_data, text_column = "abstract")

  result1 <- create_term_document_matrix(preprocessed, min_df = 1, max_df = 1.0)

  expect_true(is.matrix(result1))
  expect_true(nrow(result1) > 0)

  if (any(rowSums(result1 > 0) >= 2)) {
    result2 <- create_term_document_matrix(preprocessed, min_df = 2, max_df = 1.0)
    expect_true(is.matrix(result2))
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

  result <- suppressWarnings(
    extract_entities_workflow(
      text_data,
      entity_types = c("disease"),
      dictionary_sources = "local",
      verbose = FALSE
    )
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow with parallel processing disabled", {
  text_data <- create_test_data()

  result <- extract_entities_workflow(
    text_data,
    entity_types = c("disease"),
    dictionary_sources = "local",
    parallel = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow with caching", {
  text_data <- create_test_data()

  result1 <- extract_entities_workflow(
    text_data,
    entity_types = c("disease"),
    dictionary_sources = "local",
    cache_dictionaries = TRUE,
    verbose = FALSE
  )

  # Second call should use cache
  result2 <- extract_entities_workflow(
    text_data,
    entity_types = c("disease"),
    dictionary_sources = "local",
    cache_dictionaries = TRUE,
    verbose = FALSE
  )

  expect_s3_class(result1, "data.frame")
  expect_s3_class(result2, "data.frame")
})

test_that("extract_entities_workflow handles batch processing", {
  large_data <- data.frame(
    doc_id = 1:10,
    abstract = rep("Migraine causes headache", 10)
  )

  result <- extract_entities_workflow(
    large_data,
    entity_types = c("disease"),
    dictionary_sources = "local",
    batch_size = 5,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
})

# ============================================================================
# Test: map_ontology
# ============================================================================
test_that("map_ontology handles empty terms", {
  result <- map_ontology(
    character(0),
    ontology = "mesh"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("map_ontology requires API key for UMLS", {
  expect_error(
    map_ontology(c("headache"), ontology = "umls", api_key = NULL),
    "API key is required"
  )
})

# ============================================================================
# Test: extract_ner
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

test_that("extract_ner handles missing dictionaries gracefully", {
  text <- c("Test text")

  # This should load dummy dictionaries and not fail
  result <- suppressWarnings(
    extract_ner(text, entity_types = c("disease"))
  )

  expect_s3_class(result, "data.frame")
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

  preprocessed <- preprocess_text(text_data, text_column = "abstract")
  expect_s3_class(preprocessed, "data.frame")

  dictionary <- create_test_dictionary()
  entities <- extract_entities(preprocessed, dictionary = dictionary, sanitize_dict = FALSE)
  expect_s3_class(entities, "data.frame")

  clean_dict <- sanitize_dictionary(dictionary, verbose = FALSE)
  expect_s3_class(clean_dict, "data.frame")
  expect_true(nrow(clean_dict) <= nrow(dictionary))
})

test_that("workflow with entity extraction and topic modeling", {
  text_data <- create_test_data()

  entities <- extract_entities_workflow(
    text_data,
    entity_types = c("disease", "drug"),
    dictionary_sources = "local",
    verbose = FALSE
  )

  topics <- extract_topics(text_data, n_topics = 2)

  expect_s3_class(entities, "data.frame")
  expect_type(topics, "list")
})

# ============================================================================
# Edge Cases and Error Handling
# ============================================================================
test_that("functions handle NULL inputs gracefully", {
  expect_error(suppressWarnings(preprocess_text(NULL)))

  expect_error(extract_entities(create_test_data(), dictionary = NULL))

  result <- suppressWarnings(sanitize_dictionary(NULL, verbose = FALSE))
  expect_true(is.null(result) || (is.data.frame(result) && nrow(result) == 0))
})

test_that("functions handle empty strings", {
  text_data <- data.frame(abstract = c("", "  ", "\n"))
  result <- preprocess_text(text_data, text_column = "abstract")

  expect_s3_class(result, "data.frame")

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
  expect_true(difftime(end_time, start_time, units = "secs") < 30)
})

# ============================================================================
# Additional coverage tests for uncovered code
# ============================================================================

test_that("extract_entities handles multiple overlapping matches", {
  text_data <- data.frame(
    doc_id = 1,
    abstract = "headache pain headache severe headache"
  )

  dictionary <- data.frame(
    term = c("headache", "severe headache", "pain", "headache pain"),
    type = rep("symptom", 4),
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

test_that("sanitize_dictionary validates specific entity types", {
  # Test gene validation
  gene_dict <- data.frame(
    term = c("BRCA1", "TP53", "shortterm", "receptor kinase"),
    type = rep("gene", 4),
    id = paste0("GENE_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(gene_dict, validate_types = TRUE, verbose = FALSE)
  expect_s3_class(result, "data.frame")

  # Test drug validation
  drug_dict <- data.frame(
    term = c("aspirin", "ibuprofen", "randomword", "antibiotic"),
    type = rep("drug", 4),
    id = paste0("DRUG_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(drug_dict, validate_types = TRUE, verbose = FALSE)
  expect_s3_class(result, "data.frame")

  # Test pathway validation
  pathway_dict <- data.frame(
    term = c("glycolysis", "signaling pathway", "randomterm", "metabolism"),
    type = rep("pathway", 4),
    id = paste0("PATH_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(pathway_dict, validate_types = TRUE, verbose = FALSE)
  expect_s3_class(result, "data.frame")

  # Test method validation
  method_dict <- data.frame(
    term = c("hplc", "pcr", "randommethod", "elisa"),
    type = rep("method", 4),
    id = paste0("METH_", 1:4),
    source = rep("test", 4),
    stringsAsFactors = FALSE
  )

  result <- sanitize_dictionary(method_dict, validate_types = TRUE, verbose = FALSE)
  expect_s3_class(result, "data.frame")
})

test_that("extract_entities_workflow handles expanded entity types", {
  text_data <- create_test_data()

  result <- extract_entities_workflow(
    text_data,
    entity_types = c("disease", "protein", "symptom"),
    dictionary_sources = "local",
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
})

test_that("extract_ngrams handles trigrams and higher", {
  text <- c("This is a test sentence for ngram extraction")

  trigrams <- extract_ngrams(text, n = 3, min_freq = 1)
  expect_s3_class(trigrams, "data.frame")

  fourgrams <- extract_ngrams(text, n = 4, min_freq = 1)
  expect_s3_class(fourgrams, "data.frame")
})

test_that("extract_ngrams handles text shorter than n", {
  text <- c("short")

  result <- extract_ngrams(text, n = 3, min_freq = 1)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("segment_sentences handles various punctuation", {
  text <- "First sentence! Second sentence? Third sentence. Fourth: sentence; Fifth sentence..."

  result <- segment_sentences(text)

  expect_type(result, "list")
  expect_true(length(result[[1]]) >= 3)
})

# ============================================================================
# Run all tests
# ============================================================================
cat("\n=== Text Preprocessing Tests Complete ===\n")
cat("All tests for text_preprocessing.R functions have been executed.\n")
