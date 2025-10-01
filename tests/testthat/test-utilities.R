# tests/testthat/test-utils.R
# Comprehensive test suite for utils.R functions

library(testthat)
library(LBDiscover)

# Test data setup ----
create_test_articles <- function(n = 5) {
  data.frame(
    pmid = as.character(1001:1005)[1:n],
    title = paste("Article", 1:n),
    abstract = paste("Abstract for article", 1:n),
    authors = c("Smith J, Jones A", "Brown B", "Davis C, Wilson D",
                "Taylor E", "Anderson F")[1:n],
    publication_year = as.character(2020:2024)[1:n],
    journal = paste("Journal", 1:n),
    stringsAsFactors = FALSE
  )
}

create_test_list_articles <- function() {
  list(
    list(pmid = "1001", title = "Article 1", abstract = "Abstract 1",
         authors = c("Smith J", "Jones A"), publication_year = "2020",
         journal = "Journal 1"),
    list(pmid = "1002", title = "Article 2", abstract = "Abstract 2",
         authors = c("Brown B"), publication_year = "2021",
         journal = "Journal 2")
  )
}

# Tests for list_to_df ----
test_that("list_to_df converts valid list to data frame", {
  articles_list <- create_test_list_articles()
  result <- list_to_df(articles_list)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("pmid", "title", "abstract", "authors",
                    "publication_year", "journal") %in% colnames(result)))
  expect_equal(result$pmid[1], "1001")
  expect_equal(result$authors[1], "Smith J, Jones A")
})

test_that("list_to_df handles empty list", {
  result <- list_to_df(list())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("list_to_df handles NULL values", {
  articles_list <- list(
    list(pmid = "1001", title = NULL, abstract = "Abstract 1",
         authors = NULL, publication_year = "2020", journal = "Journal 1")
  )
  result <- list_to_df(articles_list)

  expect_true(is.na(result$title[1]))
  expect_true(is.na(result$authors[1]))
})

test_that("list_to_df errors on non-list input", {
  # Test with a character vector (atomic vector)
  expect_error(list_to_df("not a list"), "operator is invalid|Input must be a list")

  # The function will error when trying to access $ on atomic vectors
  # This is expected behavior - the input validation happens implicitly
})

test_that("list_to_df handles list with missing fields", {
  articles_list <- list(
    list(pmid = "1001", title = "Article 1")
    # Missing abstract, authors, publication_year, journal
  )
  result <- list_to_df(articles_list)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$abstract[1]))
  expect_true(is.na(result$authors[1]))
})

# Tests for save_results and load_results ----
test_that("save_results and load_results work with CSV", {
  skip_on_cran()
  results <- create_test_articles(3)
  temp_file <- tempfile(fileext = ".csv")

  # Use withr for cleanup
  withr::defer(unlink(temp_file))

  save_results(results, temp_file, format = "csv")
  expect_true(file.exists(temp_file))

  loaded <- load_results(temp_file)
  expect_equal(nrow(loaded), nrow(results))
  expect_equal(loaded$pmid, results$pmid)
})

test_that("save_results and load_results work with RDS", {
  skip_on_cran()
  results <- create_test_articles(3)
  temp_file <- tempfile(fileext = ".rds")

  withr::defer(unlink(temp_file))

  save_results(results, temp_file, format = "rds")
  expect_true(file.exists(temp_file))

  loaded <- load_results(temp_file)
  expect_equal(nrow(loaded), nrow(results))
  expect_equal(loaded$pmid, results$pmid)
})

test_that("save_results warns on extension mismatch", {
  skip_on_cran()
  results <- create_test_articles(2)
  temp_file <- tempfile(fileext = ".txt")

  file_to_remove <- paste0(tools::file_path_sans_ext(temp_file), ".csv")
  withr::defer(unlink(file_to_remove))

  expect_warning(
    save_results(results, temp_file, format = "csv"),
    "File extension does not match format"
  )
})

test_that("load_results errors on missing file", {
  expect_error(load_results("nonexistent_file.csv"), "File not found")
})

test_that("load_results errors on unsupported format", {
  skip_on_cran()
  temp_file <- tempfile(fileext = ".txt")
  writeLines("test", temp_file)
  withr::defer(unlink(temp_file))

  expect_error(load_results(temp_file), "Unsupported file format")
})

# Tests for merge_results ----
test_that("merge_results combines multiple data frames", {
  df1 <- create_test_articles(2)
  df2 <- create_test_articles(3)
  df2$pmid <- as.character(2001:2003)

  merged <- merge_results(df1, df2, remove_duplicates = FALSE)
  expect_equal(nrow(merged), 5)
})

test_that("merge_results removes duplicates by PMID", {
  df1 <- create_test_articles(3)
  df2 <- df1[1:2, ]

  merged <- merge_results(df1, df2, remove_duplicates = TRUE)
  expect_equal(nrow(merged), 3)
})

test_that("merge_results handles empty data frames", {
  df1 <- create_test_articles(2)
  df2 <- data.frame()

  expect_warning(merged <- merge_results(df1, df2))
  expect_equal(nrow(merged), 2)
})

test_that("merge_results errors on non-data-frame input", {
  expect_error(merge_results("not a df", data.frame(x = 1)),
               "All inputs must be data frames")
})

# Tests for calc_bibliometrics ----
test_that("calc_bibliometrics calculates basic statistics", {
  articles <- create_test_articles(5)
  stats <- calc_bibliometrics(articles, by_year = TRUE)

  expect_type(stats, "list")
  expect_equal(stats$total_articles, 5)
  expect_true("top_journals" %in% names(stats))
  expect_true("articles_by_year" %in% names(stats))
})

test_that("calc_bibliometrics handles missing columns gracefully", {
  articles <- data.frame(
    pmid = c("1", "2"),
    title = c("A", "B")
  )

  stats <- calc_bibliometrics(articles, by_year = FALSE)
  expect_equal(stats$total_articles, 2)
  expect_false("top_journals" %in% names(stats))
})

test_that("calc_bibliometrics errors on empty data", {
  expect_error(calc_bibliometrics(data.frame()), "article_data is empty")
})

test_that("calc_bibliometrics calculates author statistics", {
  articles <- create_test_articles(5)
  stats <- calc_bibliometrics(articles)

  expect_true("top_authors" %in% names(stats))
  expect_true("avg_authors_per_paper" %in% names(stats))
})

# Tests for extract_terms ----
test_that("extract_terms extracts common terms", {
  articles <- data.frame(
    abstract = c("migraine headache pain", "headache treatment pain relief",
                 "migraine disorder severe headache"),
    stringsAsFactors = FALSE
  )

  terms <- extract_terms(articles, text_column = "abstract", n = 10)

  expect_s3_class(terms, "data.frame")
  expect_true("word" %in% colnames(terms))
  expect_true("n" %in% colnames(terms))
  expect_true("headache" %in% terms$word)
})

test_that("extract_terms removes stopwords", {
  articles <- data.frame(
    abstract = c("the migraine and headache", "the treatment for pain"),
    stringsAsFactors = FALSE
  )

  terms <- extract_terms(articles, text_column = "abstract",
                         remove_stopwords = TRUE)

  expect_false("the" %in% terms$word)
  expect_false("and" %in% terms$word)
})

test_that("extract_terms applies minimum word length", {
  articles <- data.frame(
    abstract = c("a big headache in my head"),
    stringsAsFactors = FALSE
  )

  terms <- extract_terms(articles, text_column = "abstract",
                         min_word_length = 4)

  expect_false(any(nchar(terms$word) < 4))
})

test_that("extract_terms errors on missing column", {
  articles <- data.frame(title = c("A", "B"))
  expect_error(extract_terms(articles, text_column = "abstract"),
               "not found in the data")
})

# Tests for compare_terms ----
test_that("compare_terms compares two corpora", {
  corpus1 <- data.frame(
    abstract = c("migraine headache pain", "severe migraine"),
    stringsAsFactors = FALSE
  )
  corpus2 <- data.frame(
    abstract = c("headache treatment", "pain relief therapy"),
    stringsAsFactors = FALSE
  )

  comparison <- compare_terms(corpus1, corpus2,
                              corpus1_name = "Migraine",
                              corpus2_name = "Treatment")

  expect_s3_class(comparison, "data.frame")
  expect_true("word" %in% colnames(comparison))
  expect_true("Migraine" %in% colnames(comparison))
  expect_true("Treatment" %in% colnames(comparison))
  expect_true("ratio" %in% colnames(comparison))
})

# Tests for get_term_vars ----
test_that("get_term_vars extracts term variations", {
  articles <- data.frame(
    abstract = c("Migraine headaches are common",
                 "Migraines affect quality of life",
                 "Migraine disorders require treatment"),
    stringsAsFactors = FALSE
  )

  variations <- get_term_vars(articles, "migrain")

  expect_type(variations, "character")
  expect_true(any(grepl("migrain", variations, ignore.case = TRUE)))
  expect_true(length(variations) > 0)
})

test_that("get_term_vars handles no matches", {
  articles <- data.frame(
    abstract = c("headache pain", "treatment therapy"),
    stringsAsFactors = FALSE
  )

  variations <- get_term_vars(articles, "migrain")
  expect_equal(length(variations), 0)
})

# Tests for merge_entities ----
test_that("merge_entities combines entity datasets", {
  custom_entities <- data.frame(
    doc_id = c(1, 1),
    entity = c("migraine", "headache"),
    entity_type = c("disease", "symptom"),
    start_pos = c(1, 10),
    end_pos = c(8, 18),
    sentence = c("sent1", "sent1"),
    frequency = c(2, 1),
    stringsAsFactors = FALSE
  )

  standard_entities <- data.frame(
    doc_id = c(1, 2),
    entity = c("serotonin", "migraine"),
    entity_type = c("chemical", "disease"),
    start_pos = c(20, 1),
    end_pos = c(29, 8),
    sentence = c("sent1", "sent2"),
    frequency = c(1, 1),
    stringsAsFactors = FALSE
  )

  merged <- merge_entities(custom_entities, standard_entities,
                           primary_term = "migraine", verbose = FALSE)

  expect_s3_class(merged, "data.frame")
  expect_true(nrow(merged) >= 2)
  expect_true("migraine" %in% merged$entity)
  expect_true("serotonin" %in% merged$entity)
})

test_that("merge_entities handles NULL inputs", {
  entities <- data.frame(
    doc_id = 1,
    entity = "migraine",
    entity_type = "disease",
    start_pos = 1,
    end_pos = 8,
    sentence = "sent",
    frequency = 1,
    stringsAsFactors = FALSE
  )

  merged <- merge_entities(NULL, entities, primary_term = "migraine",
                           verbose = FALSE)
  expect_equal(nrow(merged), 1)

  merged2 <- merge_entities(entities, NULL, primary_term = "migraine",
                            verbose = FALSE)
  expect_equal(nrow(merged2), 1)
})

test_that("merge_entities creates placeholder when both NULL", {
  merged <- merge_entities(NULL, NULL, primary_term = "migraine",
                           verbose = FALSE)

  expect_s3_class(merged, "data.frame")
  expect_equal(nrow(merged), 1)
  expect_equal(merged$entity[1], "migraine")
})

# Tests for find_term ----
test_that("find_term finds exact match", {
  terms <- c("migraine", "headache", "pain")
  co_matrix <- matrix(runif(9), nrow = 3, ncol = 3)
  rownames(co_matrix) <- colnames(co_matrix) <- terms

  found <- find_term(co_matrix, "migraine", verbose = FALSE)
  expect_equal(found, "migraine")
})

test_that("find_term finds variation", {
  terms <- c("migraines", "headache", "pain")
  co_matrix <- matrix(runif(9), nrow = 3, ncol = 3)
  rownames(co_matrix) <- colnames(co_matrix) <- terms

  found <- find_term(co_matrix, "migrain", verbose = FALSE)
  expect_equal(found, "migraines")
})

test_that("find_term errors when term not found", {
  terms <- c("headache", "pain")
  co_matrix <- matrix(runif(4), nrow = 2, ncol = 2)
  rownames(co_matrix) <- colnames(co_matrix) <- terms

  expect_error(find_term(co_matrix, "migraine", verbose = FALSE),
               "missing from co-occurrence matrix")
})

# Tests for safe_diversify ----
test_that("safe_diversify diversifies results", {
  top_results <- data.frame(
    a_term = rep("migraine", 6),
    b_term = c("serotonin", "serotonin", "CGRP", "CGRP", "cortisol", "dopamine"),
    c_term = c("sumatriptan", "rizatriptan", "fremanezumab",
               "galcanezumab", "propranolol", "amitriptyline"),
    abc_score = c(0.8, 0.75, 0.7, 0.65, 0.6, 0.55),
    stringsAsFactors = FALSE
  )

  diverse <- safe_diversify(top_results, max_per_group = 2, verbose = FALSE)

  expect_s3_class(diverse, "data.frame")
  expect_true(nrow(diverse) <= nrow(top_results))
})

test_that("safe_diversify handles errors gracefully", {
  # Create malformed data that would cause diversify_abc to fail
  top_results <- data.frame(
    a_term = "migraine",
    b_term = "serotonin",
    c_term = "sumatriptan",
    abc_score = 0.8,
    stringsAsFactors = FALSE
  )

  # Should not error, but fall back to top results
  result <- safe_diversify(top_results, verbose = FALSE)
  expect_s3_class(result, "data.frame")
})

# Tests for min_results ----
test_that("min_results ensures minimum results", {
  diverse_results <- data.frame()
  top_results <- data.frame(
    a_term = rep("migraine", 3),
    b_term = c("serotonin", "CGRP", "cortisol"),
    c_term = c("sumatriptan", "fremanezumab", "propranolol"),
    abc_score = c(0.8, 0.7, 0.6),
    stringsAsFactors = FALSE
  )

  result <- min_results(diverse_results, top_results, "migraine",
                        min_results = 3, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 3)
})

test_that("min_results creates placeholder when needed", {
  result <- min_results(data.frame(), data.frame(), "migraine",
                        verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(result$a_term[1], "migraine")
  expect_true(nrow(result) >= 3)
})

# Tests for prep_articles ----
test_that("prep_articles validates publication years", {
  articles <- data.frame(
    title = c("Article 1", "Article 2", "Article 3"),
    publication_year = c("2020", "not_a_year", "2022"),
    stringsAsFactors = FALSE
  )

  prepared <- prep_articles(articles, verbose = FALSE)

  expect_equal(nrow(prepared), 2)
  expect_true(all(!is.na(prepared$publication_year)))
})

test_that("prep_articles handles NULL input", {
  result <- prep_articles(NULL, verbose = FALSE)
  expect_null(result)
})

test_that("prep_articles handles missing publication_year column", {
  articles <- data.frame(title = c("A", "B"))
  result <- prep_articles(articles, verbose = FALSE)
  expect_equal(result, articles)
})

# Tests for plot_heatmap and plot_network ----
test_that("plot_heatmap creates file", {
  skip_on_cran()
  skip_if_not_installed("graphics")

  results <- data.frame(
    a_term = rep("migraine", 4),
    b_term = c("serotonin", "CGRP", "cortisol", "dopamine"),
    c_term = c("sumatriptan", "fremanezumab", "propranolol", "amitriptyline"),
    abc_score = c(0.8, 0.7, 0.6, 0.5),
    b_type = c("chemical", "protein", "hormone", "chemical"),
    c_type = rep("drug", 4),
    stringsAsFactors = FALSE
  )

  temp_file <- tempfile(fileext = ".png")
  withr::defer(unlink(temp_file))

  # Don't check for silence - just check that file is created
  plot_heatmap(results, output_file = temp_file, verbose = FALSE)
  expect_true(file.exists(temp_file))
})

test_that("plot_network creates file", {
  skip_on_cran()
  skip_if_not_installed("igraph")

  results <- data.frame(
    a_term = rep("migraine", 4),
    b_term = c("serotonin", "CGRP", "cortisol", "dopamine"),
    c_term = c("sumatriptan", "fremanezumab", "propranolol", "amitriptyline"),
    abc_score = c(0.8, 0.7, 0.6, 0.5),
    b_type = c("chemical", "protein", "hormone", "chemical"),
    c_type = rep("drug", 4),
    stringsAsFactors = FALSE
  )

  temp_file <- tempfile(fileext = ".png")
  withr::defer(unlink(temp_file))

  # Don't check for silence - just check that file is created
  plot_network(results, output_file = temp_file, verbose = FALSE)
  expect_true(file.exists(temp_file))
})

# Tests for gen_report ----
test_that("gen_report creates HTML file", {
  skip_on_cran()

  results_list <- list(
    abc_results = data.frame(
      a_term = "migraine",
      c_term = "sumatriptan",
      abc_score = 0.8,
      stringsAsFactors = FALSE
    )
  )

  temp_file <- tempfile(fileext = ".html")
  withr::defer(unlink(temp_file))

  result <- gen_report(results_list, output_file = temp_file, verbose = FALSE)

  expect_true(file.exists(temp_file))
  expect_equal(result, temp_file)

  # Check file content
  content <- readLines(temp_file)
  expect_true(any(grepl("Discovery", content, ignore.case = TRUE)))
})

# Edge case tests ----
test_that("Functions handle edge cases appropriately", {
  # Empty strings
  articles_empty <- data.frame(
    abstract = c("", "  ", NA),
    stringsAsFactors = FALSE
  )

  terms <- extract_terms(articles_empty, text_column = "abstract")
  expect_equal(nrow(terms), 0)

  # Single row
  single_article <- create_test_articles(1)
  stats <- calc_bibliometrics(single_article)
  expect_equal(stats$total_articles, 1)
})
