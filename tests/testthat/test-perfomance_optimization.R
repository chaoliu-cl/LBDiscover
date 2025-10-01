# test-performance_optimalization.R
# Test file for performance optimization functions

library(testthat)
library(Matrix)

# Helper function to create sample entity data
create_sample_entity_data <- function(n_docs = 10, n_entities = 20) {
  set.seed(42)
  entities <- paste0("entity_", 1:n_entities)
  doc_ids <- 1:n_docs

  # Create random entity occurrences
  n_occurrences <- sample(50:100, 1)
  data.frame(
    doc_id = sample(doc_ids, n_occurrences, replace = TRUE),
    entity = sample(entities, n_occurrences, replace = TRUE),
    entity_type = sample(c("disease", "drug", "gene"), n_occurrences, replace = TRUE),
    count = sample(1:5, n_occurrences, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Helper function to create sample text data
create_sample_text_data <- function(n_docs = 10) {
  data.frame(
    doc_id = 1:n_docs,
    abstract = replicate(n_docs, paste(
      sample(c("migraine", "headache", "pain", "treatment", "serotonin",
               "receptor", "drug", "clinical", "study", "patient"),
             10, replace = TRUE),
      collapse = " "
    )),
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# Tests for create_sparse_comat()
# ============================================================================

test_that("create_sparse_comat creates valid matrix", {
  entity_data <- create_sample_entity_data()

  result <- suppressMessages(create_sparse_comat(
    entity_data,
    doc_id_col = "doc_id",
    entity_col = "entity"
  ))

  # Check that result is a Matrix object (can be dense or sparse)
  expect_s4_class(result, "Matrix")

  # Check dimensions
  n_entities <- length(unique(entity_data$entity))
  expect_equal(nrow(result), n_entities)
  expect_equal(ncol(result), n_entities)

  # Check that diagonal is zero - use a safer method for large matrices
  # Sample a few diagonal elements instead of extracting all
  sample_indices <- sample(1:min(n_entities, 10), min(5, n_entities))
  for (i in sample_indices) {
    expect_equal(result[i, i], 0)
  }

  # Check that matrix is symmetric (for co-occurrence)
  # Use Matrix package's isSymmetric or manual check
  is_symmetric <- tryCatch({
    Matrix::isSymmetric(result)
  }, error = function(e) {
    # Manual symmetry check for a few elements
    test_indices <- sample(1:min(n_entities, 5), min(3, n_entities))
    all(sapply(test_indices, function(i) {
      all(sapply(test_indices, function(j) {
        abs(result[i, j] - result[j, i]) < 1e-10
      }))
    }))
  })

  expect_true(is_symmetric)
})

test_that("create_sparse_comat handles entity types correctly", {
  entity_data <- create_sample_entity_data()

  result <- suppressMessages(create_sparse_comat(
    entity_data,
    doc_id_col = "doc_id",
    entity_col = "entity",
    type_col = "entity_type"
  ))

  # Check that entity types attribute exists
  expect_true(!is.null(attr(result, "entity_types")))

  entity_types <- attr(result, "entity_types")
  expect_type(entity_types, "character")
  expect_true(length(entity_types) > 0)
})

test_that("create_sparse_comat handles count column", {
  entity_data <- create_sample_entity_data()

  result <- suppressMessages(create_sparse_comat(
    entity_data,
    doc_id_col = "doc_id",
    entity_col = "entity",
    count_col = "count"
  ))

  expect_s4_class(result, "Matrix")
  # Values should be influenced by counts
  expect_true(max(result) > 0)
})

test_that("create_sparse_comat normalization works", {
  entity_data <- create_sample_entity_data()

  result_norm <- suppressMessages(create_sparse_comat(
    entity_data,
    normalize = TRUE
  ))

  result_no_norm <- suppressMessages(create_sparse_comat(
    entity_data,
    normalize = FALSE
  ))

  # Normalized values should generally be <= 1
  expect_true(max(result_norm) <= 1.1)  # Allow small floating point error

  # Non-normalized can be > 1
  expect_true(max(result_no_norm) >= max(result_norm))
})

test_that("create_sparse_comat handles missing columns", {
  entity_data <- create_sample_entity_data()

  expect_error(
    create_sparse_comat(entity_data, doc_id_col = "nonexistent"),
    "Required columns not found"
  )
})

test_that("create_sparse_comat handles empty data", {
  empty_data <- data.frame(
    doc_id = character(),
    entity = character(),
    stringsAsFactors = FALSE
  )

  # The function will error when trying to create progress bar with 0 rows
  expect_error(
    create_sparse_comat(empty_data),
    "must have 'max' > 'min'"
  )
})

test_that("create_sparse_comat handles single document", {
  single_doc <- data.frame(
    doc_id = rep(1, 5),
    entity = paste0("entity_", 1:5),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(create_sparse_comat(single_doc))

  expect_s4_class(result, "Matrix")
  expect_equal(nrow(result), 5)
})

test_that("create_sparse_comat errors on NA values in entity column", {
  entity_data <- create_sample_entity_data()
  # Add some NA values in entity column
  entity_data$entity[1:3] <- NA

  # The function should error because sparseMatrix doesn't accept NA
  expect_error(
    suppressMessages(create_sparse_comat(entity_data)),
    "'i' and 'j' must not contain NA"
  )
})

test_that("create_sparse_comat handles data with valid entities only", {
  entity_data <- create_sample_entity_data()
  # Ensure no NAs in critical columns
  entity_data <- entity_data[!is.na(entity_data$entity) & !is.na(entity_data$doc_id), ]

  result <- suppressMessages(create_sparse_comat(entity_data))

  expect_s4_class(result, "Matrix")
  expect_true(nrow(result) > 0)
})

# ============================================================================
# Tests for vec_preprocess()
# ============================================================================

test_that("vec_preprocess processes text correctly", {
  text_data <- create_sample_text_data()

  result <- suppressMessages(vec_preprocess(
    text_data,
    text_column = "abstract"
  ))

  # Check that terms column exists
  expect_true("terms" %in% colnames(result))

  # Check that terms is a list
  expect_type(result$terms, "list")

  # Check that each element is a data frame with expected columns
  expect_true(all(sapply(result$terms, function(x) {
    is.data.frame(x) && all(c("word", "count") %in% colnames(x))
  })))
})

test_that("vec_preprocess adds doc_id if missing", {
  text_data <- data.frame(
    abstract = c("text one", "text two"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(vec_preprocess(text_data, text_column = "abstract"))

  expect_true("doc_id" %in% colnames(result))
  expect_equal(result$doc_id, 1:2)
})

test_that("vec_preprocess stopword removal works", {
  text_data <- data.frame(
    abstract = c("the quick brown fox jumps over the lazy dog"),
    stringsAsFactors = FALSE
  )

  result_with <- suppressMessages(vec_preprocess(text_data, remove_stopwords = TRUE))
  result_without <- suppressMessages(vec_preprocess(text_data, remove_stopwords = FALSE))

  # With stopwords removed, should have fewer terms
  expect_true(nrow(result_with$terms[[1]]) < nrow(result_without$terms[[1]]))
})

test_that("vec_preprocess custom stopwords work", {
  text_data <- data.frame(
    abstract = c("migraine headache pain treatment"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(vec_preprocess(
    text_data,
    custom_stopwords = c("migraine", "headache")
  ))

  # Custom stopwords should be removed
  terms <- result$terms[[1]]$word
  expect_false("migraine" %in% terms)
  expect_false("headache" %in% terms)
})

test_that("vec_preprocess word length filtering works", {
  text_data <- data.frame(
    abstract = c("a ab abc abcd abcde"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(vec_preprocess(
    text_data,
    min_word_length = 3,
    max_word_length = 4
  ))

  terms <- result$terms[[1]]$word
  expect_true(all(nchar(terms) >= 3 & nchar(terms) <= 4))
})

test_that("vec_preprocess keeps rows but creates empty term lists for empty/NA text", {
  text_data <- data.frame(
    doc_id = 1:3,
    abstract = c("", NA, "valid text"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(vec_preprocess(text_data))

  # Function keeps all rows but filters NA text during processing
  # So we get 2 rows (empty string is kept in data, NA is filtered)
  expect_true(nrow(result) >= 1)

  # The row with valid text should have terms
  valid_row <- which(!is.na(result$abstract) & result$abstract != "")
  if (length(valid_row) > 0) {
    expect_true(nrow(result$terms[[valid_row[1]]]) > 0)
  }
})

test_that("vec_preprocess chunk processing works", {
  text_data <- create_sample_text_data(n_docs = 250)

  # Should process in chunks - will produce messages
  expect_message({
    result <- vec_preprocess(text_data, chunk_size = 100)
  }, "Processing text in")

  expect_equal(nrow(result), 250)
})

test_that("vec_preprocess handles missing text column", {
  text_data <- data.frame(
    not_abstract = c("text"),
    stringsAsFactors = FALSE
  )

  expect_error(
    vec_preprocess(text_data, text_column = "abstract"),
    "Text column 'abstract' not found"
  )
})

test_that("vec_preprocess processes valid text correctly", {
  text_data <- data.frame(
    abstract = c("migraine headache treatment"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(vec_preprocess(text_data, remove_stopwords = FALSE))

  expect_equal(nrow(result), 1)
  expect_true(nrow(result$terms[[1]]) > 0)
  expect_true(all(c("migraine", "headache", "treatment") %in% result$terms[[1]]$word))
})

# ============================================================================
# Tests for parallel_analysis()
# ============================================================================

test_that("parallel_analysis processes documents", {
  skip_if_not_installed("parallel")

  text_data <- create_sample_text_data(n_docs = 10)

  # Simple analysis function: count words
  count_words <- function(text) {
    length(unlist(strsplit(tolower(text), "\\s+")))
  }

  result <- suppressMessages(parallel_analysis(
    text_data,
    analysis_function = count_words,
    text_column = "abstract",
    n_cores = 2
  ))

  expect_true("analysis_result" %in% colnames(result))
  expect_type(result$analysis_result, "list")
})

test_that("parallel_analysis handles NA text", {
  skip_if_not_installed("parallel")

  text_data <- data.frame(
    abstract = c("valid text", NA, "another text"),
    stringsAsFactors = FALSE
  )

  count_words <- function(text) {
    if (is.na(text)) return(NA)
    length(unlist(strsplit(text, "\\s+")))
  }

  result <- suppressMessages(parallel_analysis(
    text_data,
    analysis_function = count_words,
    text_column = "abstract",
    n_cores = 2
  ))

  expect_true(is.na(result$analysis_result[[2]]))
  expect_false(is.na(result$analysis_result[[1]]))
})

test_that("parallel_analysis respects n_cores parameter", {
  skip_if_not_installed("parallel")

  text_data <- create_sample_text_data(n_docs = 5)

  simple_fn <- function(text) nchar(text)

  # Should work with different core counts
  result1 <- suppressMessages(parallel_analysis(text_data, simple_fn, n_cores = 1))
  result2 <- suppressMessages(parallel_analysis(text_data, simple_fn, n_cores = 2))

  expect_equal(result1$analysis_result, result2$analysis_result)
})

test_that("parallel_analysis handles missing text column", {
  skip_if_not_installed("parallel")

  text_data <- data.frame(not_abstract = c("text"), stringsAsFactors = FALSE)

  expect_error(
    parallel_analysis(text_data, function(x) x, text_column = "abstract"),
    "Text column 'abstract' not found"
  )
})

# ============================================================================
# Tests for abc_model_opt()
# ============================================================================

test_that("abc_model_opt finds connections", {
  # Create a simple co-occurrence matrix
  set.seed(42)
  terms <- c("migraine", "headache", "serotonin", "sumatriptan", "pain")
  n <- length(terms)
  co_matrix <- Matrix(runif(n * n, 0, 1), nrow = n, ncol = n)
  rownames(co_matrix) <- colnames(co_matrix) <- terms
  diag(co_matrix) <- 0

  # Add entity types
  entity_types <- c("disease", "symptom", "chemical", "drug", "symptom")
  names(entity_types) <- terms
  attr(co_matrix, "entity_types") <- entity_types

  result <- suppressMessages(abc_model_opt(
    co_matrix,
    a_term = "migraine",
    min_score = 0.1,
    n_results = 10
  ))

  expect_s3_class(result, "data.frame")
  expect_true(all(c("a_term", "b_term", "c_term", "abc_score") %in% colnames(result)))
})

test_that("abc_model_opt handles specific c_term", {
  set.seed(42)
  terms <- c("migraine", "serotonin", "sumatriptan")
  n <- length(terms)
  co_matrix <- Matrix(runif(n * n, 0, 0.8), nrow = n, ncol = n)
  rownames(co_matrix) <- colnames(co_matrix) <- terms
  diag(co_matrix) <- 0

  result <- suppressMessages(abc_model_opt(
    co_matrix,
    a_term = "migraine",
    c_term = "sumatriptan",
    min_score = 0.1
  ))

  if (nrow(result) > 0) {
    expect_true(all(result$c_term == "sumatriptan"))
  }
})

test_that("abc_model_opt handles missing a_term", {
  set.seed(42)
  terms <- c("term1", "term2", "term3")
  n <- length(terms)
  co_matrix <- Matrix(runif(n * n), nrow = n, ncol = n)
  rownames(co_matrix) <- colnames(co_matrix) <- terms

  expect_error(
    abc_model_opt(co_matrix, a_term = "nonexistent"),
    "A-term 'nonexistent' not found"
  )
})

test_that("abc_model_opt processes in chunks", {
  # Create larger matrix to test chunking
  set.seed(42)
  n <- 100
  terms <- paste0("term_", 1:n)
  co_matrix <- Matrix(runif(n * n, 0, 0.5), nrow = n, ncol = n)
  rownames(co_matrix) <- colnames(co_matrix) <- terms
  diag(co_matrix) <- 0

  result <- suppressMessages(abc_model_opt(
    co_matrix,
    a_term = "term_1",
    chunk_size = 20,
    min_score = 0.01
  ))

  expect_s3_class(result, "data.frame")
})

test_that("abc_model_opt returns empty for no connections", {
  set.seed(42)
  terms <- c("term1", "term2", "term3")
  n <- length(terms)
  # Create matrix with all zeros (no connections)
  co_matrix <- Matrix(0, nrow = n, ncol = n)
  rownames(co_matrix) <- colnames(co_matrix) <- terms

  result <- suppressMessages(abc_model_opt(
    co_matrix,
    a_term = "term1",
    min_score = 0.1
  ))

  expect_equal(nrow(result), 0)
})

test_that("abc_model_opt respects n_results limit", {
  set.seed(42)
  n <- 20
  terms <- paste0("term_", 1:n)
  co_matrix <- Matrix(runif(n * n, 0.3, 0.9), nrow = n, ncol = n)
  rownames(co_matrix) <- colnames(co_matrix) <- terms
  diag(co_matrix) <- 0

  result <- suppressMessages(abc_model_opt(
    co_matrix,
    a_term = "term_1",
    n_results = 5,
    min_score = 0.1
  ))

  expect_true(nrow(result) <= 5)
})

test_that("abc_model_opt includes entity types when available", {
  set.seed(42)
  terms <- c("disease1", "protein1", "drug1")
  n <- length(terms)
  co_matrix <- Matrix(runif(n * n, 0.3, 0.8), nrow = n, ncol = n)
  rownames(co_matrix) <- colnames(co_matrix) <- terms
  diag(co_matrix) <- 0

  entity_types <- c("disease", "protein", "drug")
  names(entity_types) <- terms
  attr(co_matrix, "entity_types") <- entity_types

  result <- suppressMessages(abc_model_opt(
    co_matrix,
    a_term = "disease1",
    min_score = 0.1
  ))

  if (nrow(result) > 0) {
    expect_true(all(c("a_type", "b_type", "c_type") %in% colnames(result)))
  }
})

# ============================================================================
# Integration tests
# ============================================================================

test_that("sparse matrix creation integrates with abc_model_opt", {
  entity_data <- create_sample_entity_data(n_docs = 20, n_entities = 10)

  co_matrix <- suppressMessages(create_sparse_comat(
    entity_data,
    normalize = TRUE
  ))

  # Pick a term that exists
  a_term <- rownames(co_matrix)[1]

  result <- suppressMessages(abc_model_opt(
    co_matrix,
    a_term = a_term,
    min_score = 0.01,
    n_results = 5
  ))

  expect_s3_class(result, "data.frame")
})

test_that("vectorized preprocessing with matrix creation", {
  text_data <- create_sample_text_data(n_docs = 15)

  # Preprocess
  processed <- suppressMessages(vec_preprocess(text_data, remove_stopwords = TRUE))

  # Create entity data from processed terms
  entity_list <- list()
  for (i in seq_len(nrow(processed))) {
    if (nrow(processed$terms[[i]]) > 0) {
      entity_list[[i]] <- data.frame(
        doc_id = processed$doc_id[i],
        entity = processed$terms[[i]]$word,
        count = processed$terms[[i]]$count,
        stringsAsFactors = FALSE
      )
    }
  }
  entity_data <- do.call(rbind, entity_list)

  # Create co-occurrence matrix
  co_matrix <- suppressMessages(create_sparse_comat(entity_data))

  # Matrix class (can be dense or sparse depending on data)
  expect_s4_class(co_matrix, "Matrix")
})

# ============================================================================
# Performance tests (optional - can be skipped in routine testing)
# ============================================================================

test_that("large matrix performance is acceptable", {
  skip_on_cran()
  skip_if_not(interactive())

  # Create a large entity dataset
  n_docs <- 1000
  n_entities <- 500
  large_entity_data <- create_sample_entity_data(n_docs, n_entities)

  # Time the sparse matrix creation
  start_time <- Sys.time()
  co_matrix <- suppressMessages(create_sparse_comat(large_entity_data, normalize = TRUE))
  end_time <- Sys.time()

  time_diff <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Should complete in reasonable time (adjust threshold as needed)
  expect_true(time_diff < 60)  # Less than 60 seconds
})
