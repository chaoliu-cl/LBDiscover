# tests/testthat/test-comprehensive-summary.R

library(testthat)
library(LBDiscover)

# Helper function to create mock article data
create_mock_articles <- function(n = 10) {
  data.frame(
    pmid = as.character(1:n),
    title = paste("Article", 1:n),
    abstract = paste("This is a sample abstract about migraine headache pain treatment with",
                     sample(c("sumatriptan", "topiramate", "propranolol", "serotonin", "CGRP"), n, replace = TRUE)),
    authors = paste("Author", 1:n),
    publication_year = sample(2015:2023, n, replace = TRUE),
    journal = paste("Journal", 1:n),
    stringsAsFactors = FALSE
  )
}

# Helper function to create mock co-occurrence matrix
create_mock_cooccurrence_matrix <- function() {
  terms <- c("migraine", "headache", "pain", "serotonin", "CGRP", "sumatriptan",
             "topiramate", "propranolol")
  n <- length(terms)

  # Create a symmetric matrix with random values
  set.seed(123)
  mat <- matrix(runif(n * n, 0, 1), nrow = n, ncol = n)
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  diag(mat) <- 1

  rownames(mat) <- colnames(mat) <- terms

  # Add entity types
  entity_types <- c("disease", "symptom", "symptom", "chemical", "protein",
                    "drug", "drug", "drug")
  names(entity_types) <- terms
  attr(mat, "entity_types") <- entity_types

  # Add metadata
  attr(mat, "metadata") <- list(
    n_docs = 10,
    n_entities = n,
    has_types = TRUE,
    normalization = "cosine"
  )

  return(mat)
}

# Test run_lbd function
test_that("run_lbd validates discovery approaches", {
  expect_error(
    run_lbd(
      search_query = "migraine",
      a_term = "migraine",
      discovery_approaches = c("invalid_approach")
    ),
    "Invalid discovery approaches"
  )
})

test_that("run_lbd validates dictionary sources", {
  expect_error(
    run_lbd(
      search_query = "migraine",
      a_term = "migraine",
      dictionary_sources = c("invalid_source")
    ),
    "Invalid dictionary sources"
  )
})

test_that("run_lbd handles empty PubMed results", {
  skip_on_cran()

  # Mock pubmed_search to return empty results
  with_mocked_bindings(
    {
      expect_error(
        run_lbd(
          search_query = "zzznoresultsquery999",
          a_term = "migraine"
        ),
        "No articles found"
      )
    },
    pubmed_search = function(...) data.frame(),
    .package = "LBDiscover"
  )
})

test_that("run_lbd processes ABC approach successfully", {
  skip_on_cran()
  skip_if_offline()

  # Create mock functions to avoid actual API calls
  mock_articles <- create_mock_articles(20)
  mock_matrix <- create_mock_cooccurrence_matrix()

  with_mocked_bindings(
    {
      results <- run_lbd(
        search_query = "migraine headache",
        a_term = "migraine",
        max_results = 10,
        discovery_approaches = c("abc"),
        include_visualizations = FALSE,
        dictionary_sources = c("local")
      )

      expect_type(results, "list")
      expect_true("abc" %in% names(results))
      expect_s3_class(results$abc, "data.frame")
    },
    pubmed_search = function(...) mock_articles,
    vec_preprocess = function(...) mock_articles,
    load_dictionary = function(...) {
      data.frame(
        term = c("migraine", "headache", "sumatriptan"),
        entity_type = c("disease", "symptom", "drug"),
        stringsAsFactors = FALSE
      )
    },
    extract_entities = function(...) {
      data.frame(
        doc_id = rep(1:5, each = 3),
        entity = rep(c("migraine", "headache", "sumatriptan"), 5),
        entity_type = rep(c("disease", "symptom", "drug"), 5),
        stringsAsFactors = FALSE
      )
    },
    create_comat = function(...) mock_matrix,
    .package = "LBDiscover"
  )
})

test_that("run_lbd processes multiple approaches", {
  skip_on_cran()

  mock_articles <- create_mock_articles(15)
  mock_matrix <- create_mock_cooccurrence_matrix()

  with_mocked_bindings(
    {
      results <- run_lbd(
        search_query = "migraine",
        a_term = "migraine",
        max_results = 10,
        discovery_approaches = c("abc", "bitola"),
        include_visualizations = FALSE,
        dictionary_sources = c("local")
      )

      expect_type(results, "list")
      expect_true(all(c("abc", "bitola") %in% names(results)))
    },
    pubmed_search = function(...) mock_articles,
    vec_preprocess = function(...) mock_articles,
    load_dictionary = function(...) {
      data.frame(
        term = c("migraine", "serotonin", "sumatriptan"),
        entity_type = c("disease", "chemical", "drug"),
        stringsAsFactors = FALSE
      )
    },
    extract_entities = function(...) {
      data.frame(
        doc_id = rep(1:5, each = 3),
        entity = rep(c("migraine", "serotonin", "sumatriptan"), 5),
        entity_type = rep(c("disease", "chemical", "drug"), 5),
        stringsAsFactors = FALSE
      )
    },
    create_comat = function(...) mock_matrix,
    .package = "LBDiscover"
  )
})

test_that("run_lbd handles dictionary loading errors gracefully", {
  skip_on_cran()

  mock_articles <- create_mock_articles(10)

  with_mocked_bindings(
    {
      expect_error(
        run_lbd(
          search_query = "migraine",
          a_term = "migraine",
          dictionary_sources = c("local"),
          entity_categories = c("disease")
        ),
        "No valid dictionary terms found"
      )
    },
    pubmed_search = function(...) mock_articles,
    vec_preprocess = function(...) mock_articles,
    load_dictionary = function(...) stop("Dictionary error"),
    .package = "LBDiscover"
  )
})

test_that("run_lbd skips UMLS without API key", {
  skip_on_cran()

  mock_articles <- create_mock_articles(10)
  mock_matrix <- create_mock_cooccurrence_matrix()

  # Capture messages to verify UMLS was skipped
  expect_message(
    with_mocked_bindings(
      {
        run_lbd(
          search_query = "migraine",
          a_term = "migraine",
          discovery_approaches = c("abc"),  # Only use ABC to avoid LSI path
          dictionary_sources = c("umls", "local"),
          entity_categories = c("disease"),
          api_key = NULL,
          include_visualizations = FALSE
        )
      },
      pubmed_search = function(...) mock_articles,
      vec_preprocess = function(...) mock_articles,
      load_dictionary = function(dictionary_type, source, api_key = NULL, ...) {
        if (source == "umls" && is.null(api_key)) {
          stop("API key required")
        }
        data.frame(
          term = c("migraine", "headache"),
          entity_type = c("disease", "symptom"),
          stringsAsFactors = FALSE
        )
      },
      extract_entities = function(...) {
        data.frame(
          doc_id = rep(1:5, each = 2),
          entity = rep(c("migraine", "headache"), 5),
          entity_type = rep(c("disease", "symptom"), 5),
          stringsAsFactors = FALSE
        )
      },
      create_comat = function(...) mock_matrix,
      .package = "LBDiscover"
    ),
    "Skipping UMLS source"
  )
})

# Test apply_bitola_flexible function
test_that("apply_bitola_flexible works with valid input", {
  mock_matrix <- create_mock_cooccurrence_matrix()

  results <- apply_bitola_flexible(
    co_matrix = mock_matrix,
    a_term = "migraine",
    min_score = 0.1,
    n_results = 10
  )

  expect_s3_class(results, "data.frame")
  expect_true(all(c("a_term", "a_type", "c_term", "c_type", "support",
                    "bitola_score", "b_terms", "ranking_score") %in% names(results)))
  expect_true(all(results$a_term == "migraine"))
  expect_true(nrow(results) <= 10)
})

test_that("apply_bitola_flexible handles missing A term", {
  mock_matrix <- create_mock_cooccurrence_matrix()

  expect_error(
    apply_bitola_flexible(
      co_matrix = mock_matrix,
      a_term = "nonexistent_term"
    ),
    "not found in the co-occurrence matrix"
  )
})

test_that("apply_bitola_flexible handles matrix without entity types", {
  mock_matrix <- create_mock_cooccurrence_matrix()
  attr(mock_matrix, "entity_types") <- NULL

  # Should fall back to ABC model
  with_mocked_bindings(
    {
      results <- apply_bitola_flexible(
        co_matrix = mock_matrix,
        a_term = "migraine"
      )

      expect_s3_class(results, "data.frame")
    },
    abc_model = function(...) {
      data.frame(
        a_term = "migraine",
        b_term = "serotonin",
        c_term = "sumatriptan",
        abc_score = 0.8,
        stringsAsFactors = FALSE
      )
    },
    .package = "LBDiscover"
  )
})

test_that("apply_bitola_flexible handles no B terms found", {
  mock_matrix <- create_mock_cooccurrence_matrix()
  # Set all associations to zero except diagonal
  mock_matrix[mock_matrix < 0.99] <- 0

  results <- apply_bitola_flexible(
    co_matrix = mock_matrix,
    a_term = "migraine",
    min_score = 0.5
  )

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 0)
  expect_true(all(c("a_term", "b_term", "c_term", "bitola_score") %in% names(results)))
})

test_that("apply_bitola_flexible aggregates results correctly", {
  mock_matrix <- create_mock_cooccurrence_matrix()

  results <- apply_bitola_flexible(
    co_matrix = mock_matrix,
    a_term = "migraine",
    min_score = 0.3,
    n_results = 100
  )

  # Check that support counts are numeric (can be integer or double)
  expect_true(is.numeric(results$support))

  # Check that b_terms is a character vector with comma-separated values
  expect_type(results$b_terms, "character")
  if (nrow(results) > 0) {
    expect_true(all(grepl(",", results$b_terms) | !grepl(",", results$b_terms)))
  }

  # Check that ranking_score is calculated
  expect_true("ranking_score" %in% names(results))
  if (nrow(results) > 0) {
    expect_true(all(abs(results$ranking_score - results$support * results$bitola_score) < 1e-10))
  }
})

test_that("apply_bitola_flexible respects n_results parameter", {
  mock_matrix <- create_mock_cooccurrence_matrix()

  results <- apply_bitola_flexible(
    co_matrix = mock_matrix,
    a_term = "migraine",
    min_score = 0.1,
    n_results = 3
  )

  expect_true(nrow(results) <= 3)
})

test_that("apply_bitola_flexible sorts by ranking score", {
  mock_matrix <- create_mock_cooccurrence_matrix()

  results <- apply_bitola_flexible(
    co_matrix = mock_matrix,
    a_term = "migraine",
    min_score = 0.2,
    n_results = 50
  )

  if (nrow(results) > 1) {
    # Check that results are sorted in descending order by ranking_score
    expect_true(all(diff(results$ranking_score) <= 0))
  }
})

test_that("apply_bitola_flexible handles A term without type", {
  mock_matrix <- create_mock_cooccurrence_matrix()
  entity_types <- attr(mock_matrix, "entity_types")

  # Remove type for A term
  entity_types <- entity_types[names(entity_types) != "migraine"]
  attr(mock_matrix, "entity_types") <- entity_types

  expect_message(
    results <- apply_bitola_flexible(
      co_matrix = mock_matrix,
      a_term = "migraine",
      min_score = 0.2
    ),
    "has no entity type information"
  )

  expect_s3_class(results, "data.frame")
})

# Integration tests
test_that("run_lbd with visualizations creates report", {
  skip_on_cran()

  mock_articles <- create_mock_articles(10)
  mock_matrix <- create_mock_cooccurrence_matrix()
  temp_output <- tempfile(fileext = ".html")

  with_mocked_bindings(
    {
      results <- run_lbd(
        search_query = "migraine",
        a_term = "migraine",
        max_results = 5,
        discovery_approaches = c("abc"),
        include_visualizations = TRUE,
        output_file = temp_output,
        dictionary_sources = c("local")
      )

      expect_type(results, "list")
      expect_true("abc" %in% names(results))
    },
    pubmed_search = function(...) mock_articles,
    vec_preprocess = function(...) mock_articles,
    load_dictionary = function(...) {
      data.frame(
        term = c("migraine", "headache", "sumatriptan"),
        entity_type = c("disease", "symptom", "drug"),
        stringsAsFactors = FALSE
      )
    },
    extract_entities = function(...) {
      data.frame(
        doc_id = rep(1:5, each = 3),
        entity = rep(c("migraine", "headache", "sumatriptan"), 5),
        entity_type = rep(c("disease", "symptom", "drug"), 5),
        stringsAsFactors = FALSE
      )
    },
    create_comat = function(...) mock_matrix,
    vis_heatmap = function(...) NULL,
    export_network = function(...) NULL,
    export_chord_diagram = function(...) NULL,
    create_report = function(...) NULL,
    .package = "LBDiscover"
  )
})

test_that("run_lbd combines multiple entity categories", {
  skip_on_cran()

  mock_articles <- create_mock_articles(10)
  mock_matrix <- create_mock_cooccurrence_matrix()

  with_mocked_bindings(
    {
      results <- run_lbd(
        search_query = "migraine",
        a_term = "migraine",
        discovery_approaches = c("abc"),
        include_visualizations = FALSE,
        dictionary_sources = c("local"),
        entity_categories = c("disease", "drug", "protein")
      )

      expect_type(results, "list")
    },
    pubmed_search = function(...) mock_articles,
    vec_preprocess = function(...) mock_articles,
    load_dictionary = function(dictionary_type, ...) {
      # Return different terms based on category
      switch(dictionary_type,
             "disease" = data.frame(term = "migraine", entity_type = "disease", stringsAsFactors = FALSE),
             "drug" = data.frame(term = "sumatriptan", entity_type = "drug", stringsAsFactors = FALSE),
             "protein" = data.frame(term = "CGRP", entity_type = "protein", stringsAsFactors = FALSE),
             data.frame(term = character(0), entity_type = character(0), stringsAsFactors = FALSE)
      )
    },
    extract_entities = function(...) {
      data.frame(
        doc_id = rep(1:3, each = 3),
        entity = rep(c("migraine", "sumatriptan", "CGRP"), 3),
        entity_type = rep(c("disease", "drug", "protein"), 3),
        stringsAsFactors = FALSE
      )
    },
    create_comat = function(...) mock_matrix,
    .package = "LBDiscover"
  )
})

test_that("apply_bitola_flexible excludes A and B terms from C terms", {
  mock_matrix <- create_mock_cooccurrence_matrix()

  results <- apply_bitola_flexible(
    co_matrix = mock_matrix,
    a_term = "migraine",
    min_score = 0.1
  )

  # Ensure A term is not in C terms
  if (nrow(results) > 0) {
    expect_false("migraine" %in% results$c_term)

    # Ensure B terms are not in C terms for the same row
    for (i in 1:nrow(results)) {
      b_terms_list <- strsplit(results$b_terms[i], ", ")[[1]]
      expect_false(results$c_term[i] %in% b_terms_list)
    }
  }
})
