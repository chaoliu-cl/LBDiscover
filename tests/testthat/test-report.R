# Tests for report generation functions

# Create mock ABC results for testing
create_mock_abc_results <- function(n_results = 50) {
  set.seed(123)  # For reproducibility

  # Generate A, B, and C terms
  a_terms <- paste0("A_Term_", 1:2)
  b_terms <- paste0("B_Term_", 1:10)
  c_terms <- paste0("C_Term_", 1:8)

  # Generate entity types
  entity_types <- c("disease", "drug", "gene", "protein", "pathway")

  # Create results data frame
  results <- data.frame(
    a_term = sample(a_terms, n_results, replace = TRUE),
    b_term = sample(b_terms, n_results, replace = TRUE),
    c_term = sample(c_terms, n_results, replace = TRUE),
    a_b_score = runif(n_results, 0.1, 0.9),
    b_c_score = runif(n_results, 0.1, 0.9),
    abc_score = runif(n_results, 0.01, 0.5),
    stringsAsFactors = FALSE
  )

  # Add entity types
  results$a_type <- sample(entity_types, n_results, replace = TRUE)
  results$b_type <- sample(entity_types, n_results, replace = TRUE)
  results$c_type <- sample(entity_types, n_results, replace = TRUE)

  # Add significance values
  results$p_value <- runif(n_results, 0, 0.1)
  results$significant <- results$p_value < 0.05

  # Ensure there are no duplicate A-B-C combinations
  results <- unique(results[, c("a_term", "b_term", "c_term", "a_b_score", "b_c_score", "abc_score",
                                "a_type", "b_type", "c_type", "p_value", "significant")])

  return(results)
}

# Create mock ANC results for testing
create_mock_anc_results <- function(n_results = 30) {
  set.seed(456)  # Different seed from ABC

  # Generate A and C terms
  a_terms <- paste0("A_Term_", 1:2)
  c_terms <- paste0("C_Term_", 1:12)

  # Generate entity types
  entity_types <- c("disease", "drug", "gene", "protein", "pathway")

  # Create results data frame
  results <- data.frame(
    a_term = sample(a_terms, n_results, replace = TRUE),
    c_term = sample(c_terms, n_results, replace = TRUE),
    shared_b_terms = sample(5:15, n_results, replace = TRUE),
    anc_score = runif(n_results, 0.01, 0.5),
    stringsAsFactors = FALSE
  )

  # Add entity types
  results$a_type <- sample(entity_types, n_results, replace = TRUE)
  results$c_type <- sample(entity_types, n_results, replace = TRUE)

  # Add example B terms
  b_terms <- paste0("B_Term_", 1:10)
  results$example_b_terms <- sapply(1:n_results, function(i) {
    paste(sample(b_terms, sample(3:5, 1)), collapse = ", ")
  })

  # Ensure there are no duplicate A-C combinations
  results <- unique(results[, c("a_term", "c_term", "shared_b_terms", "anc_score",
                                "a_type", "c_type", "example_b_terms")])

  return(results)
}

# Create mock articles for testing
create_mock_articles <- function(n_articles = 100) {
  set.seed(789)  # Different seed

  # Create article IDs
  article_ids <- paste0("PMID", sample(10000:99999, n_articles))

  # Create publication years
  pub_years <- sample(2000:2023, n_articles, replace = TRUE)

  # Create sample titles and abstracts
  sample_titles <- c(
    "Effects of compound X on receptor Y activation",
    "Gene expression analysis in disease Z",
    "Novel drug delivery system for treating condition W",
    "Molecular mechanisms of protein P in signaling pathway S",
    "Clinical trial results for therapy T in patient population P",
    "Structural analysis of enzyme E and its substrates",
    "Biomarker discovery for early detection of disease D",
    "Computational models of metabolic pathway M",
    "Side effects of drug D in long-term treatment",
    "Genetic variants associated with disease susceptibility"
  )

  sample_abstracts <- c(
    "Background: This study investigated... Methods: We analyzed... Results: The data showed... Conclusion: Our findings suggest...",
    "Objective: To determine... Materials: Samples were collected... Analysis: Statistical tests revealed... Discussion: These results indicate...",
    "Introduction: Previous research has shown... Approach: We developed... Outcomes: Significant improvements were observed... Summary: This work demonstrates...",
    "Purpose: The aim was to investigate... Experimental design: A randomized trial... Findings: Treatment resulted in... Interpretation: These data support..."
  )

  # Generate titles and abstracts
  titles <- sapply(1:n_articles, function(i) {
    base_title <- sample(sample_titles, 1)
    # Add some random variation
    paste(base_title, "- Study", i)
  })

  abstracts <- sapply(1:n_articles, function(i) {
    base_abstract <- sample(sample_abstracts, 1)
    # Add some random variation
    paste(base_abstract, "Additional analysis revealed interesting findings related to study parameters.")
  })

  # Create journal names
  sample_journals <- c(
    "Journal of Biomedical Research",
    "Clinical Investigations",
    "Molecular Medicine Reports",
    "Drug Discovery Today",
    "Bioinformatics Advances",
    "Scientific Reports",
    "Nature Methods",
    "Cell Research",
    "PLOS One",
    "BMC Genomics"
  )

  journals <- sample(sample_journals, n_articles, replace = TRUE)

  # Create authors
  sample_authors <- c(
    "Smith J, Jones A, Brown T",
    "Zhang Y, Chen H, Wang L",
    "Kumar R, Patel S, Singh A",
    "Müller W, Schmidt H, Fischer K",
    "García M, Rodriguez C, López J",
    "Kim S, Park J, Lee H",
    "Johnson R, Williams M, Davis L",
    "Nguyen T, Tran H, Pham V",
    "Anderson P, Taylor S, Wilson R",
    "Ivanov A, Petrov N, Smirnov K"
  )

  authors <- sample(sample_authors, n_articles, replace = TRUE)

  # Combine into articles data frame
  articles <- data.frame(
    doc_id = article_ids,
    publication_year = pub_years,
    title = titles,
    abstract = abstracts,
    journal = journals,
    authors = authors,
    stringsAsFactors = FALSE
  )

  return(articles)
}

# Test create_report function
test_that("create_report generates an HTML report file", {
  # Create mock results
  abc_results <- create_mock_abc_results(30)
  anc_results <- create_mock_anc_results(20)

  # Package results in a list
  results_list <- list(
    abc = abc_results,
    anc = anc_results
  )

  # Create mock articles
  articles <- create_mock_articles(50)

  # Create mock visualization paths
  vis_paths <- list(
    heatmap = tempfile(fileext = ".png"),
    network = tempfile(fileext = ".html"),
    chord = tempfile(fileext = ".html")
  )

  # Create dummy files for visualization paths
  writeLines("Test content", vis_paths$heatmap)
  writeLines("<html><body>Test</body></html>", vis_paths$network)
  writeLines("<html><body>Test</body></html>", vis_paths$chord)

  # Create temporary file for output
  output_file <- tempfile(fileext = ".html")

  # Test report generation
  expect_silent(create_report(
    results = results_list,
    visualizations = vis_paths,
    articles = articles,
    output_file = output_file
  ))

  # Check that file exists and has content
  expect_true(file.exists(output_file))
  expect_gt(file.size(output_file), 0)

  # Check that file contains key report elements
  file_content <- readLines(output_file, warn = FALSE)
  content_text <- paste(file_content, collapse = "\n")

  # Check for section headings
  expect_match(content_text, "Literature-Based Discovery Report")
  expect_match(content_text, "Overview")
  expect_match(content_text, "ABC Results")
  expect_match(content_text, "ANC Results")
  expect_match(content_text, "Visualizations")
  expect_match(content_text, "Data Summary")

  # Check for visualization links
  expect_match(content_text, "Open Network Visualization")
  expect_match(content_text, "Open Chord Diagram")

  # Check for data summary
  expect_match(content_text, "Publication Years Distribution")

  # Clean up
  unlink(output_file)
  unlink(vis_paths$heatmap)
  unlink(vis_paths$network)
  unlink(vis_paths$chord)
})

# Test report generation with partial data
test_that("create_report handles partial data gracefully", {
  # Create mock results with only one approach
  abc_results <- create_mock_abc_results(30)

  # Package results in a list
  results_list <- list(
    abc = abc_results
  )

  # Create temporary file for output
  output_file <- tempfile(fileext = ".html")

  # Test report generation with only results, no visualizations or articles
  expect_silent(create_report(
    results = results_list,
    output_file = output_file
  ))

  # Check that file exists and has content
  expect_true(file.exists(output_file))
  expect_gt(file.size(output_file), 0)

  # File should contain ABC results but not ANC
  file_content <- readLines(output_file, warn = FALSE)
  content_text <- paste(file_content, collapse = "\n")
  expect_match(content_text, "ABC Results")
  expect_false(grepl("ANC Results", content_text))

  # Clean up
  unlink(output_file)

  # Test with just articles, no results or visualizations
  articles <- create_mock_articles(50)
  output_file2 <- tempfile(fileext = ".html")

  expect_silent(create_report(
    results = list(),
    articles = articles,
    output_file = output_file2
  ))

  # Check that file exists and has content
  expect_true(file.exists(output_file2))
  expect_gt(file.size(output_file2), 0)

  # Clean up
  unlink(output_file2)
})

# Test error handling in report generation
test_that("create_report handles problematic input gracefully", {
  # Create mock results with empty data frame
  empty_results <- list(
    abc = data.frame(
      a_term = character(0),
      b_term = character(0),
      c_term = character(0),
      abc_score = numeric(0)
    )
  )

  # Test report generation with empty results
  output_file <- tempfile(fileext = ".html")
  expect_silent(create_report(
    results = empty_results,
    output_file = output_file
  ))

  # File should still be created
  expect_true(file.exists(output_file))

  # Clean up
  unlink(output_file)

  # Test with non-existing visualization paths
  vis_paths <- list(
    heatmap = "/path/does/not/exist.png",
    network = "/path/does/not/exist.html"
  )

  output_file2 <- tempfile(fileext = ".html")
  expect_silent(create_report(
    results = list(abc = create_mock_abc_results(5)),
    visualizations = vis_paths,
    output_file = output_file2
  ))

  # File should still be created
  expect_true(file.exists(output_file2))

  # Clean up
  unlink(output_file2)
})

# Test with problematic publication years
test_that("create_report handles problematic publication years", {
  # Create articles with non-numeric years
  articles <- create_mock_articles(50)

  # Introduce some problematic years
  articles$publication_year[1:5] <- "Unknown"
  articles$publication_year[6:10] <- NA
  articles$publication_year[11:15] <- ""

  # Test report generation
  output_file <- tempfile(fileext = ".html")
  expect_silent(create_report(
    results = list(abc = create_mock_abc_results(5)),
    articles = articles,
    output_file = output_file
  ))

  # File should be created
  expect_true(file.exists(output_file))

  # Clean up
  unlink(output_file)
})

# Test gen_report function (internal version)
test_that("gen_report function works as expected", {
  # Skip if function doesn't exist (it's internal)
  skip_if_not(exists("gen_report"), "gen_report function not found")

  # Create mock data
  results_list <- list(
    abc = create_mock_abc_results(10)
  )

  vis_paths <- list(
    heatmap = tempfile(fileext = ".png")
  )
  writeLines("Test content", vis_paths$heatmap)

  articles <- create_mock_articles(20)

  # Create temporary file for output
  output_file <- tempfile(fileext = ".html")

  # Test function - don't expect silence, just no errors
  expect_no_error(gen_report(
    results = results_list,
    visualizations = vis_paths,
    articles = articles,
    output_file = output_file
  ))

  # Check that file exists
  expect_true(file.exists(output_file))

  # Clean up
  unlink(output_file)
  unlink(vis_paths$heatmap)
})
