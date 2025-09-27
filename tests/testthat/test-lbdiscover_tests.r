library(testthat)

# ============================================================================
# Tests for calculate_score (existing, enhanced)
# ============================================================================
test_that("calculate_score handles edge cases", {
  # Test with zero scores
  expect_equal(calculate_score(0, 0, "multiplication"), 0)
  expect_equal(calculate_score(0, 1, "average"), 0.5)

  # Test with identical scores
  expect_equal(calculate_score(0.5, 0.5, "multiplication"), 0.25)
  expect_equal(calculate_score(0.5, 0.5, "average"), 0.5)

  # Test with boundary values
  expect_equal(calculate_score(1, 1, "multiplication"), 1)
  expect_equal(calculate_score(1, 1, "average"), 1)

  # Test combined method
  combined_score <- calculate_score(0.6, 0.8, "combined")
  expect_true(combined_score > 0 && combined_score < 1)
})

# ============================================================================
# Tests for validation functions (existing, enhanced)
# ============================================================================
test_that("validation functions handle various inputs", {
  # Test with valid biomedical terms
  expect_true(is_valid_biomedical_entity("migraine", "disease"))
  expect_true(is_valid_biomedical_entity("receptor", "protein"))

  # Test with invalid terms
  expect_false(is_valid_biomedical_entity("optimization", "disease"))
  expect_false(is_valid_biomedical_entity("europe", "gene"))

  # Test with edge cases
  expect_false(is_valid_biomedical_entity("", "disease"))
  expect_false(is_valid_biomedical_entity("123", "protein"))
  expect_false(is_valid_biomedical_entity(NULL, "disease"))
})

# ============================================================================
# Tests for diversify_b_terms (existing, enhanced)
# ============================================================================
test_that("diversify_b_terms works correctly", {
  # Create test data
  test_results <- data.frame(
    a_term = rep("A", 9),
    b_term = rep(c("B1", "B2", "B3"), each = 3),
    c_term = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"),
    a_b_score = rep(0.5, 9),
    b_c_score = rep(0.6, 9),
    abc_score = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
    stringsAsFactors = FALSE
  )

  # Apply diversification
  diverse_results <- diversify_b_terms(test_results, max_per_group = 2)

  # Check that we have at most 2 results per B term
  expect_true(all(table(diverse_results$b_term) <= 2))

  # Test with empty input
  empty_results <- data.frame(
    a_term = character(),
    b_term = character(),
    c_term = character(),
    abc_score = numeric()
  )
  expect_equal(nrow(diversify_b_terms(empty_results)), 0)
})

# ============================================================================
# Tests for list_to_df
# ============================================================================
test_that("list_to_df converts articles correctly", {
  # Test with valid article list
  articles <- list(
    list(pmid = "12345", title = "Test Article", abstract = "Abstract text",
         authors = c("Smith J", "Doe J"), publication_year = "2020",
         journal = "Test Journal"),
    list(pmid = "67890", title = "Another Article", abstract = "More text",
         authors = c("Jones A"), publication_year = "2021",
         journal = "Another Journal")
  )

  result <- list_to_df(articles)
  expect_equal(nrow(result), 2)
  expect_equal(result$pmid[1], "12345")
  expect_equal(result$authors[1], "Smith J, Doe J")

  # Test with empty list
  expect_equal(nrow(list_to_df(list())), 0)

  # Test with NULL values
  articles_with_null <- list(
    list(pmid = "12345", title = NULL, abstract = "Abstract")
  )
  result_null <- list_to_df(articles_with_null)
  expect_true(is.na(result_null$title[1]))

  # Test error handling
  expect_error(list_to_df("not a list"), "Input must be a list")
})

# ============================================================================
# Tests for merge_results
# ============================================================================
test_that("merge_results combines data frames correctly", {
  df1 <- data.frame(pmid = c("1", "2"), title = c("A", "B"))
  df2 <- data.frame(pmid = c("3", "4"), title = c("C", "D"))
  df3 <- data.frame(pmid = c("2", "5"), title = c("B", "E"))

  # Test basic merge
  merged <- merge_results(df1, df2, remove_duplicates = FALSE)
  expect_equal(nrow(merged), 4)

  # Test with duplicate removal
  merged_dedup <- merge_results(df1, df3, remove_duplicates = TRUE)
  expect_equal(nrow(merged_dedup), 3)
  expect_true("5" %in% merged_dedup$pmid)

  # Test with empty data frame
  empty_df <- data.frame(pmid = character(), title = character())
  merged_empty <- merge_results(df1, empty_df)
  expect_equal(nrow(merged_empty), 2)

  # Test error handling
  expect_error(merge_results(df1, "not a df"), "All inputs must be data frames")
})

# ============================================================================
# Tests for calc_bibliometrics
# ============================================================================
test_that("calc_bibliometrics calculates statistics correctly", {
  # Create test data
  articles <- data.frame(
    pmid = c("1", "2", "3"),
    title = c("Article 1", "Article 2", "Article 3"),
    abstract = c("This is abstract one", "Another abstract here", "Third abstract text"),
    authors = c("Smith J, Doe J", "Jones A", "Smith J, Brown K"),
    publication_year = c("2020", "2020", "2021"),
    journal = c("Journal A", "Journal A", "Journal B"),
    stringsAsFactors = FALSE
  )

  stats <- calc_bibliometrics(articles, by_year = TRUE)

  # Test basic statistics
  expect_equal(stats$total_articles, 3)
  expect_true(!is.null(stats$top_journals))
  expect_true(!is.null(stats$articles_by_year))

  # Test author statistics
  expect_true(!is.null(stats$avg_authors_per_paper))
  expect_true(stats$avg_authors_per_paper > 0)

  # Test abstract statistics
  expect_true(!is.null(stats$avg_abstract_length))
  expect_true(stats$avg_abstract_length > 0)

  # Test error handling
  empty_articles <- data.frame()
  expect_error(calc_bibliometrics(empty_articles), "article_data is empty")
})

# ============================================================================
# Tests for extract_terms
# ============================================================================
test_that("extract_terms extracts and counts terms correctly", {
  # Create test data
  articles <- data.frame(
    doc_id = 1:3,
    abstract = c(
      "migraine headache pain treatment",
      "chronic migraine disorder headache",
      "pain management migraine therapy"
    ),
    stringsAsFactors = FALSE
  )

  # Test basic extraction
  terms <- extract_terms(articles, text_column = "abstract", n = 10)
  expect_true(nrow(terms) > 0)
  expect_true("migraine" %in% terms$word)

  # Test with stopword removal
  terms_no_stop <- extract_terms(articles, remove_stopwords = TRUE, min_word_length = 3)
  expect_false("the" %in% terms_no_stop$word)

  # Test with different parameters
  terms_short <- extract_terms(articles, n = 2)
  expect_equal(nrow(terms_short), 2)

  # Test error handling
  expect_error(extract_terms(articles, text_column = "nonexistent"),
               "Text column 'nonexistent' not found")
})

# ============================================================================
# Tests for compare_terms
# ============================================================================
test_that("compare_terms compares corpora correctly", {
  corpus1 <- data.frame(
    abstract = c("migraine headache treatment", "chronic migraine pain"),
    stringsAsFactors = FALSE
  )

  corpus2 <- data.frame(
    abstract = c("headache therapy medication", "treatment options available"),
    stringsAsFactors = FALSE
  )

  # Test comparison
  comparison <- compare_terms(corpus1, corpus2,
                              corpus1_name = "Migraine",
                              corpus2_name = "Headache",
                              n = 20)

  expect_true(nrow(comparison) > 0)
  expect_true("Migraine" %in% colnames(comparison))
  expect_true("Headache" %in% colnames(comparison))
  expect_true("ratio" %in% colnames(comparison))

  # Test error handling
  bad_corpus <- data.frame(text = c("test"))
  expect_error(compare_terms(corpus1, bad_corpus),
               "Text column 'abstract' not found")
})

# ============================================================================
# Tests for get_term_vars
# ============================================================================
test_that("get_term_vars extracts term variations", {
  articles <- data.frame(
    abstract = c(
      "Migraine headaches are debilitating",
      "Migraines affect quality of life",
      "Migraine disorders require treatment"
    ),
    stringsAsFactors = FALSE
  )

  variations <- get_term_vars(articles, "migrain", text_col = "abstract")

  expect_true(length(variations) > 0)
  expect_true(any(grepl("migrain", variations, ignore.case = TRUE)))

  # Test with no matches
  no_match <- get_term_vars(articles, "cancer", text_col = "abstract")
  expect_equal(length(no_match), 0)
})

# ============================================================================
# Tests for merge_entities
# ============================================================================
test_that("merge_entities combines entity datasets correctly", {
  custom_entities <- data.frame(
    doc_id = c(1, 1, 2),
    entity = c("migraine", "headache", "pain"),
    entity_type = c("disease", "symptom", "symptom"),
    start_pos = c(1, 10, 5),
    end_pos = c(8, 18, 9),
    sentence = c("sent1", "sent1", "sent2"),
    frequency = c(2, 1, 1),
    stringsAsFactors = FALSE
  )

  standard_entities <- data.frame(
    doc_id = c(1, 2, 2),
    entity = c("serotonin", "migraine", "therapy"),
    entity_type = c("chemical", "disease", "treatment"),
    start_pos = c(20, 1, 15),
    end_pos = c(29, 8, 22),
    sentence = c("sent1", "sent2", "sent2"),
    frequency = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  # Test merging both datasets
  merged <- merge_entities(custom_entities, standard_entities,
                           "migraine", verbose = FALSE)
  expect_true(nrow(merged) >= nrow(custom_entities))
  expect_true("serotonin" %in% merged$entity)

  # Test with NULL custom entities
  merged_std_only <- merge_entities(NULL, standard_entities,
                                    "migraine", verbose = FALSE)
  expect_equal(nrow(merged_std_only), nrow(standard_entities))

  # Test with NULL standard entities
  merged_custom_only <- merge_entities(custom_entities, NULL,
                                       "migraine", verbose = FALSE)
  expect_equal(nrow(merged_custom_only), nrow(custom_entities))

  # Test with both NULL
  merged_both_null <- merge_entities(NULL, NULL, "migraine", verbose = FALSE)
  expect_equal(nrow(merged_both_null), 1)
  expect_equal(merged_both_null$entity[1], "migraine")
})

# ============================================================================
# Tests for valid_entities
# ============================================================================
test_that("valid_entities filters entities correctly", {
  entities <- data.frame(
    entity = c("migraine", "optimization", "receptor", "europe"),
    entity_type = c("disease", "process", "protein", "location"),
    stringsAsFactors = FALSE
  )

  # Create a simple validation function for testing
  simple_validator <- function(term, type) {
    # Validate based on simple rules
    if (type == "disease" && term %in% c("migraine", "headache")) return(TRUE)
    if (type == "protein" && term == "receptor") return(TRUE)
    return(FALSE)
  }

  # Test filtering
  filtered <- valid_entities(entities, "migraine",
                             validation_function = simple_validator,
                             verbose = FALSE)

  expect_true(nrow(filtered) <= nrow(entities))
  expect_true("migraine" %in% filtered$entity)

  # Test with empty input
  empty_entities <- data.frame(
    entity = character(),
    entity_type = character()
  )
  filtered_empty <- valid_entities(empty_entities, "test", verbose = FALSE)
  expect_equal(nrow(filtered_empty), 0)
})

# ============================================================================
# Tests for find_term
# ============================================================================
test_that("find_term locates terms in co-occurrence matrix", {
  # Create test co-occurrence matrix
  terms <- c("migraine", "headache", "pain", "serotonin")
  co_matrix <- matrix(runif(16, 0, 1), nrow = 4, ncol = 4)
  rownames(co_matrix) <- colnames(co_matrix) <- terms

  # Test exact match
  found <- find_term(co_matrix, "migraine", verbose = FALSE)
  expect_equal(found, "migraine")

  # Test partial match
  found_partial <- find_term(co_matrix, "headach", verbose = FALSE)
  expect_true(grepl("headach", found_partial, ignore.case = TRUE))

  # Test error for missing term
  expect_error(find_term(co_matrix, "cancer", verbose = FALSE),
               "Primary term and variations missing")
})

# ============================================================================
# Tests for safe_diversify
# ============================================================================
test_that("safe_diversify handles diversification with error handling", {
  top_results <- data.frame(
    a_term = rep("migraine", 6),
    b_term = c("serotonin", "serotonin", "CGRP", "CGRP", "cortisol", "dopamine"),
    c_term = c("sumatriptan", "rizatriptan", "fremanezumab",
               "galcanezumab", "propranolol", "amitriptyline"),
    abc_score = c(0.8, 0.75, 0.7, 0.65, 0.6, 0.55),
    stringsAsFactors = FALSE
  )

  # Test basic diversification
  diverse <- safe_diversify(top_results, max_per_group = 2, verbose = FALSE)
  expect_true(nrow(diverse) > 0)

  # Test with empty input
  empty_results <- data.frame(
    a_term = character(),
    b_term = character(),
    c_term = character(),
    abc_score = numeric()
  )
  diverse_empty <- safe_diversify(empty_results, verbose = FALSE)
  expect_equal(nrow(diverse_empty), 0)
})

# ============================================================================
# Tests for min_results
# ============================================================================
test_that("min_results ensures minimum result count", {
  diverse_results <- data.frame()

  top_results <- data.frame(
    a_term = rep("migraine", 3),
    b_term = c("serotonin", "CGRP", "cortisol"),
    c_term = c("sumatriptan", "fremanezumab", "propranolol"),
    abc_score = c(0.8, 0.7, 0.6),
    stringsAsFactors = FALSE
  )

  # Test with empty diverse results
  result <- min_results(diverse_results, top_results, "migraine",
                        min_results = 3, verbose = FALSE)
  expect_true(nrow(result) >= 3)

  # Test with sufficient diverse results
  diverse_sufficient <- top_results[1:3, ]
  result_sufficient <- min_results(diverse_sufficient, top_results,
                                   "migraine", min_results = 2, verbose = FALSE)
  expect_equal(nrow(result_sufficient), 3)
})

# ============================================================================
# Tests for prep_articles
# ============================================================================
test_that("prep_articles validates publication years", {
  articles <- data.frame(
    title = c("Article 1", "Article 2", "Article 3"),
    publication_year = c("2020", "not_a_year", "2021"),
    stringsAsFactors = FALSE
  )

  # Test preparation
  prepared <- prep_articles(articles, verbose = FALSE)
  expect_true(nrow(prepared) < nrow(articles))
  expect_false(any(is.na(prepared$publication_year)))

  # Test with NULL input
  expect_null(prep_articles(NULL, verbose = FALSE))

  # Test with no publication_year column
  articles_no_year <- data.frame(title = c("A", "B"))
  result <- prep_articles(articles_no_year, verbose = FALSE)
  expect_equal(result, articles_no_year)
})

# ============================================================================
# Tests for file I/O functions
# ============================================================================
test_that("save_results and load_results work correctly", {
  skip_on_cran()

  # Create test data
  test_results <- data.frame(
    term = c("migraine", "headache"),
    score = c(0.8, 0.7),
    stringsAsFactors = FALSE
  )

  # Test CSV format
  csv_file <- tempfile(fileext = ".csv")
  save_results(test_results, csv_file, format = "csv")
  expect_true(file.exists(csv_file))

  loaded_csv <- load_results(csv_file)
  expect_equal(nrow(loaded_csv), 2)
  expect_equal(loaded_csv$term[1], "migraine")

  # Test RDS format
  rds_file <- tempfile(fileext = ".rds")
  save_results(test_results, rds_file, format = "rds")
  expect_true(file.exists(rds_file))

  loaded_rds <- load_results(rds_file)
  expect_equal(nrow(loaded_rds), 2)

  # Test error handling
  expect_error(load_results("nonexistent_file.csv"), "File not found")

  # Test unsupported format - create a dummy file with unsupported extension
  xyz_file <- tempfile(fileext = ".xyz")
  writeLines("dummy content", xyz_file)
  expect_error(load_results(xyz_file), "Unsupported file format")

  # Clean up
  unlink(c(csv_file, rds_file, xyz_file))
})

# ============================================================================
# Tests for visualization functions
# ============================================================================
test_that("vis_abc_heatmap handles input validation", {
  skip_on_cran()
  skip_if_not_installed("graphics")

  # Create test results
  abc_results <- data.frame(
    a_term = rep("migraine", 4),
    b_term = c("serotonin", "CGRP", "cortisol", "dopamine"),
    c_term = c("sumatriptan", "fremanezumab", "propranolol", "amitriptyline"),
    abc_score = c(0.8, 0.7, 0.6, 0.5),
    stringsAsFactors = FALSE
  )

  # Test that function doesn't error with valid input
  expect_silent({
    png(tempfile(fileext = ".png"))
    vis_abc_heatmap(abc_results, top_n = 4, min_score = 0.1,
                    show_labels = FALSE, title = "Test Heatmap")
    dev.off()
  })

  # Test error handling
  empty_results <- data.frame(
    a_term = character(), b_term = character(),
    c_term = character(), abc_score = numeric()
  )
  expect_error(vis_abc_heatmap(empty_results), "ABC results are empty")

  # Test with high min_score that filters everything
  expect_error(vis_abc_heatmap(abc_results, min_score = 1.0),
               "No results remain after filtering")
})

# ============================================================================
# Tests for apply_bitola_flexible
# ============================================================================
test_that("apply_bitola_flexible handles co-occurrence matrices", {
  # Create test co-occurrence matrix
  terms <- c("migraine", "serotonin", "CGRP", "sumatriptan")
  co_matrix <- matrix(c(
    1.0, 0.7, 0.6, 0.3,
    0.7, 1.0, 0.5, 0.8,
    0.6, 0.5, 1.0, 0.7,
    0.3, 0.8, 0.7, 1.0
  ), nrow = 4, ncol = 4, byrow = TRUE)
  rownames(co_matrix) <- colnames(co_matrix) <- terms

  # Add entity types
  entity_types <- c("disease", "chemical", "protein", "drug")
  names(entity_types) <- terms
  attr(co_matrix, "entity_types") <- entity_types

  # Test BITOLA model
  results <- apply_bitola_flexible(co_matrix, a_term = "migraine",
                                   min_score = 0.2, n_results = 10)

  expect_true(is.data.frame(results))
  expect_true(nrow(results) >= 0)

  if (nrow(results) > 0) {
    expect_true("a_term" %in% colnames(results))
    expect_true("c_term" %in% colnames(results))
    expect_true("bitola_score" %in% colnames(results))
  }

  # Test error handling
  expect_error(apply_bitola_flexible(co_matrix, a_term = "nonexistent"),
               "A-term 'nonexistent' not found")
})
