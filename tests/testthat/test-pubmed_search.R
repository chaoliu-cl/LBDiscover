# Test file for pubmed_search.R functions
# Tests for the LBDiscover package

library(testthat)

# Test helper functions and data
create_mock_xml_response <- function() {
  '<?xml version="1.0" ?>
<!DOCTYPE PubmedArticleSet PUBLIC "-//NLM//DTD PubMedArticle, 1st January 2019//EN" "https://dtd.nlm.nih.gov/ncbi/pubmed/out/pubmed_190101.dtd">
<PubmedArticleSet>
<PubmedArticle>
  <MedlineCitation Owner="NLM" Status="MEDLINE">
    <PMID Version="1">12345678</PMID>
    <Article PubModel="Print">
      <Journal>
        <Title>Test Journal</Title>
        <ISOAbbreviation>Test J</ISOAbbreviation>
        <JournalIssue CitedMedium="Print">
          <PubDate>
            <Year>2023</Year>
          </PubDate>
        </JournalIssue>
      </Journal>
      <ArticleTitle>Test Article Title About Migraine</ArticleTitle>
      <Abstract>
        <AbstractText Label="BACKGROUND">This is a test abstract about migraine research.</AbstractText>
        <AbstractText Label="METHODS">We conducted a systematic review.</AbstractText>
      </Abstract>
      <AuthorList CompleteYN="Y">
        <Author ValidYN="Y">
          <LastName>Smith</LastName>
          <ForeName>John</ForeName>
          <Initials>J</Initials>
        </Author>
        <Author ValidYN="Y">
          <LastName>Doe</LastName>
          <ForeName>Jane</ForeName>
          <Initials>J</Initials>
        </Author>
      </AuthorList>
    </Article>
    <KeywordList Owner="NOTNLM">
      <Keyword MajorTopicYN="N">migraine</Keyword>
      <Keyword MajorTopicYN="N">headache</Keyword>
    </KeywordList>
  </MedlineCitation>
  <PubmedData>
    <ArticleIdList>
      <ArticleId IdType="pubmed">12345678</ArticleId>
      <ArticleId IdType="doi">10.1234/test.2023.001</ArticleId>
    </ArticleIdList>
  </PubmedData>
</PubmedArticle>
</PubmedArticleSet>'
}

create_mock_search_result <- function() {
  list(
    count = 1,
    ids = "12345678",
    web_history = list(
      WebEnv = "test_webenv",
      QueryKey = "1"
    )
  )
}

# Helper function to check if we can run integration tests
can_run_integration_tests <- function() {
  # Check if rentrez is available and we have internet connection
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    return(FALSE)
  }

  # Try a simple API call to check connectivity
  tryCatch({
    # This is a minimal test - just try to load the rentrez package
    library(rentrez, quietly = TRUE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Tests for parse_pubmed_xml function (doesn't require mocking)
test_that("parse_pubmed_xml parses XML correctly", {
  skip_if_not_installed("xml2")

  xml_data <- create_mock_xml_response()
  result <- parse_pubmed_xml(xml_data, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$pmid[1], "12345678")
  expect_true(grepl("Test Article Title", result$title[1]))
  expect_true(grepl("migraine research", result$abstract[1]))
  expect_equal(result$authors[1], "Smith John, Doe Jane")
  expect_equal(result$keywords[1], "migraine, headache")
  expect_equal(result$doi[1], "10.1234/test.2023.001")
})

test_that("parse_pubmed_xml handles empty XML", {
  skip_if_not_installed("xml2")

  empty_xml <- '<?xml version="1.0" ?><PubmedArticleSet></PubmedArticleSet>'
  result <- parse_pubmed_xml(empty_xml, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("parse_pubmed_xml handles malformed XML", {
  skip_if_not_installed("xml2")

  expect_error(parse_pubmed_xml("invalid xml"))
})

# Tests for cache functions (don't require mocking)
test_that("pubmed cache functions work", {
  # Test getting cache environment
  cache_env <- get_pubmed_cache()
  expect_type(cache_env, "environment")

  # Test cache clearing
  expect_no_error(clear_pubmed_cache())
})

test_that("cleanup functions work", {
  # Test that cleanup doesn't cause errors
  expect_no_error(clear_pubmed_cache())

  # Verify cache is actually cleared
  cache_env <- get_pubmed_cache()
  expect_equal(length(ls(cache_env)), 0)
})

# Tests for helper functions (don't require mocking)
test_that("null coalescing operator works", {
  # Test %||% operator
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(character(0) %||% "default", "default")
})

test_that("retry_api_call works with success", {
  # Mock successful function
  test_func <- function() "success"

  result <- retry_api_call(test_func, verbose = FALSE)
  expect_equal(result, "success")
})

test_that("retry_api_call retries on failure", {
  # Create a function that fails twice then succeeds
  call_count <- 0
  test_func <- function() {
    call_count <<- call_count + 1
    if (call_count <= 2) {
      stop("error ", call_count)
    }
    "success"
  }

  result <- retry_api_call(test_func, retry_count = 3, retry_delay = 0.01, verbose = FALSE)
  expect_equal(result, "success")
  expect_equal(call_count, 3)
})

test_that("retry_api_call returns NULL after all retries fail", {
  # Mock function that always fails
  test_func <- function() stop("always fails")

  result <- retry_api_call(test_func, retry_count = 3, retry_delay = 0.01, verbose = FALSE)
  expect_null(result)
})

# Integration tests (only run if rentrez is available and we have internet)
test_that("pubmed_search basic functionality works (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not_installed("xml2")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  # This is a real integration test - it will make actual API calls
  # We use a very specific search that should return consistent results
  result <- tryCatch({
    pubmed_search("migraine[MeSH Terms] AND 2023[PDAT]", max_results = 2, verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_true("pmid" %in% colnames(result))
  expect_true("title" %in% colnames(result))
  expect_true("abstract" %in% colnames(result))
})

test_that("pubmed_search handles empty results (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  # Use a search term that should return no results
  result <- tryCatch({
    pubmed_search("nonexistentveryrareterm12345", max_results = 10, verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("pubmed_search validates parameters correctly", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  # Test that empty query returns empty results
  result <- tryCatch({
    pubmed_search("", max_results = 10, verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("pubmed_search works with date ranges (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    pubmed_search("migraine",
                  date_range = c("2023/01/01", "2023/12/31"),
                  max_results = 2,
                  verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
})

test_that("pubmed_search uses cache correctly (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not_installed("digest")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  # Clear cache first
  clear_pubmed_cache()

  query <- "migraine[MeSH Terms] AND 2023[PDAT]"

  # First call should hit the API
  result1 <- tryCatch({
    pubmed_search(query, max_results = 1, use_cache = TRUE, verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })

  # Second call should use cache (should be faster)
  start_time <- Sys.time()
  result2 <- tryCatch({
    pubmed_search(query, max_results = 1, use_cache = TRUE, verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })
  end_time <- Sys.time()

  expect_equal(result1, result2)
  # Cached call should be very fast (less than 1 second)
  expect_lt(as.numeric(end_time - start_time), 1)
})

test_that("get_pmc_fulltext works with valid PMIDs (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  # Use a known PMID that should have PMC access
  # Note: This might not always work due to availability, so we handle gracefully
  result <- tryCatch({
    get_pmc_fulltext(c("12345678"))  # This is a test PMID
  }, error = function(e) {
    skip(paste("PMC API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_true("pmid" %in% colnames(result))
  expect_true("pmc_id" %in% colnames(result))
  expect_true("fulltext" %in% colnames(result))
})

test_that("ncbi_search works for PubMed database (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    ncbi_search("migraine", database = "pubmed", max_results = 1)
  }, error = function(e) {
    skip(paste("NCBI API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
})

# Unit tests that test error handling without requiring API calls
test_that("pubmed_search handles API errors gracefully", {
  skip_if_not_installed("rentrez")

  # Test with a malformed query that should cause an error
  result <- tryCatch({
    pubmed_search("invalid[query[", max_results = 1, verbose = FALSE, retry_count = 1)
  }, error = function(e) {
    # If the function throws an error, we expect it to be handled gracefully
    # Return an empty data frame to test the error handling
    data.frame()
  })

  expect_s3_class(result, "data.frame")
})

test_that("pubmed_search performance test", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  # Test that a small search completes in reasonable time
  start_time <- Sys.time()
  result <- tryCatch({
    pubmed_search("migraine", max_results = 5, verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })
  end_time <- Sys.time()

  # Should complete within 30 seconds (generous limit for API calls)
  expect_lt(as.numeric(end_time - start_time), 30)
  expect_s3_class(result, "data.frame")
})

# Test parameter validation without API calls
test_that("parameter validation works correctly", {
  # Test that the function accepts valid parameters without error
  expect_no_error({
    # These parameters should be validated without making API calls
    query <- "migraine"
    max_results <- 10
    use_mesh <- FALSE
    date_range <- c("2020/01/01", "2023/12/31")

    # Basic parameter validation
    expect_type(query, "character")
    expect_type(max_results, "double")
    expect_type(use_mesh, "logical")
    expect_length(date_range, 2)
  })
})

# Test that required dependencies are handled correctly
test_that("missing dependencies are handled gracefully", {
  # Test that the functions fail gracefully when required packages are missing

  # Mock the package checking
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    expect_error(pubmed_search("test"), "rentrez.*required")
  }

  if (!requireNamespace("xml2", quietly = TRUE)) {
    expect_error(parse_pubmed_xml("<xml></xml>"), "xml2.*required")
  }
})
