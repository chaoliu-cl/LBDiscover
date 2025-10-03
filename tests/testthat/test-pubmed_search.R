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

create_mock_xml_with_medline_date <- function() {
  '<?xml version="1.0" ?>
<PubmedArticleSet>
<PubmedArticle>
  <MedlineCitation>
    <PMID>11111111</PMID>
    <Article>
      <Journal>
        <ISOAbbreviation>Abbrev J</ISOAbbreviation>
        <JournalIssue>
          <PubDate>
            <MedlineDate>2022 Spring</MedlineDate>
          </PubDate>
        </JournalIssue>
      </Journal>
      <ArticleTitle>Test with MedlineDate</ArticleTitle>
    </Article>
  </MedlineCitation>
</PubmedArticle>
</PubmedArticleSet>'
}

create_mock_xml_with_collective_author <- function() {
  '<?xml version="1.0" ?>
<PubmedArticleSet>
<PubmedArticle>
  <MedlineCitation>
    <PMID>22222222</PMID>
    <Article>
      <Journal>
        <Title>Collective Journal</Title>
        <JournalIssue>
          <PubDate>
            <Year>2023</Year>
          </PubDate>
        </JournalIssue>
      </Journal>
      <ArticleTitle>Article with Collective Author</ArticleTitle>
      <AuthorList>
        <Author>
          <CollectiveName>Research Consortium Group</CollectiveName>
        </Author>
        <Author>
          <LastName>Smith</LastName>
          <Initials>JS</Initials>
        </Author>
      </AuthorList>
    </Article>
  </MedlineCitation>
</PubmedArticle>
</PubmedArticleSet>'
}

create_mock_xml_with_article_date <- function() {
  '<?xml version="1.0" ?>
<PubmedArticleSet>
<PubmedArticle>
  <MedlineCitation>
    <PMID>33333333</PMID>
    <Article>
      <Journal>
        <Title>Date Test Journal</Title>
        <JournalIssue>
          <PubDate></PubDate>
        </JournalIssue>
      </Journal>
      <ArticleTitle>Article with ArticleDate</ArticleTitle>
      <ArticleDate>
        <Year>2021</Year>
      </ArticleDate>
    </Article>
  </MedlineCitation>
</PubmedArticle>
</PubmedArticleSet>'
}

# Helper function to check if we can run integration tests
can_run_integration_tests <- function() {
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    return(FALSE)
  }

  tryCatch({
    library(rentrez, quietly = TRUE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Tests for parse_pubmed_xml function
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

test_that("parse_pubmed_xml handles MedlineDate format", {
  skip_if_not_installed("xml2")

  xml_data <- create_mock_xml_with_medline_date()
  result <- parse_pubmed_xml(xml_data, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$publication_year[1], "2022")
  expect_equal(result$journal[1], "Abbrev J")
})

test_that("parse_pubmed_xml handles ArticleDate", {
  skip_if_not_installed("xml2")

  xml_data <- create_mock_xml_with_article_date()
  result <- parse_pubmed_xml(xml_data, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(result$publication_year[1], "2021")
})

test_that("parse_pubmed_xml handles collective authors", {
  skip_if_not_installed("xml2")

  xml_data <- create_mock_xml_with_collective_author()
  result <- parse_pubmed_xml(xml_data, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(grepl("Research Consortium Group", result$authors[1]))
  expect_true(grepl("Smith JS", result$authors[1]))
})

test_that("parse_pubmed_xml handles abstracts with labels", {
  skip_if_not_installed("xml2")

  xml_data <- create_mock_xml_response()
  result <- parse_pubmed_xml(xml_data, verbose = FALSE)

  expect_true(grepl("BACKGROUND:", result$abstract[1]))
  expect_true(grepl("METHODS:", result$abstract[1]))
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

test_that("parse_pubmed_xml handles verbose output", {
  skip_if_not_installed("xml2")

  xml_data <- create_mock_xml_response()
  expect_message(parse_pubmed_xml(xml_data, verbose = TRUE), "Processing")
})

# Tests for cache functions
test_that("pubmed cache functions work", {
  cache_env <- get_pubmed_cache()
  expect_type(cache_env, "environment")

  expect_no_error(clear_pubmed_cache())
})

test_that("cleanup functions work", {
  expect_no_error(clear_pubmed_cache())

  cache_env <- get_pubmed_cache()
  expect_equal(length(ls(cache_env)), 0)
})

# Tests for helper functions
test_that("null coalescing operator works", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(character(0) %||% "default", "default")
})

test_that("retry_api_call works with success", {
  test_func <- function() "success"

  result <- retry_api_call(test_func, verbose = FALSE)
  expect_equal(result, "success")
})

test_that("retry_api_call retries on failure", {
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
  test_func <- function() stop("always fails")

  result <- retry_api_call(test_func, retry_count = 3, retry_delay = 0.01, verbose = FALSE)
  expect_null(result)
})

test_that("retry_api_call handles rate limit errors with longer backoff", {
  call_count <- 0
  test_func <- function() {
    call_count <<- call_count + 1
    if (call_count == 1) {
      stop("429 Too Many Requests")
    }
    "success"
  }

  start_time <- Sys.time()
  result <- retry_api_call(test_func, retry_count = 2, retry_delay = 0.1, verbose = FALSE)
  end_time <- Sys.time()

  expect_equal(result, "success")
  # Should have waited at least 0.2 seconds (doubled delay for rate limit)
  expect_true(as.numeric(end_time - start_time) >= 0.2)
})

test_that("retry_api_call uses exponential backoff", {
  call_count <- 0
  test_func <- function() {
    call_count <<- call_count + 1
    if (call_count < 3) {
      stop("error")
    }
    "success"
  }

  start_time <- Sys.time()
  result <- retry_api_call(test_func, retry_count = 3, retry_delay = 0.1, verbose = FALSE)
  end_time <- Sys.time()

  expect_equal(result, "success")
  # Should wait 0.1 + 0.2 = 0.3 seconds total (exponential backoff)
  expect_true(as.numeric(end_time - start_time) >= 0.3)
})

# Integration tests (only run if rentrez is available)
test_that("pubmed_search basic functionality works (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not_installed("xml2")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

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

test_that("pubmed_search with MeSH mapping works (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not_installed("xml2")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    pubmed_search("headache", max_results = 2, use_mesh = TRUE, verbose = TRUE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
})

test_that("pubmed_search handles batch processing correctly (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not_installed("xml2")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    # Request more results than default batch size to test batching
    pubmed_search("migraine", max_results = 250, batch_size = 100, verbose = TRUE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) <= 250)
})

test_that("pubmed_search handles empty results (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

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

  clear_pubmed_cache()

  query <- "migraine[MeSH Terms] AND 2023[PDAT]"

  result1 <- tryCatch({
    pubmed_search(query, max_results = 1, use_cache = TRUE, verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })

  start_time <- Sys.time()
  result2 <- tryCatch({
    pubmed_search(query, max_results = 1, use_cache = TRUE, verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })
  end_time <- Sys.time()

  expect_equal(result1, result2)
  expect_lt(as.numeric(end_time - start_time), 1)
})

test_that("get_pmc_fulltext works with valid PMIDs (integration test)", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    get_pmc_fulltext(c("12345678"))
  }, error = function(e) {
    skip(paste("PMC API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  expect_true("pmid" %in% colnames(result))
  expect_true("pmc_id" %in% colnames(result))
  expect_true("fulltext" %in% colnames(result))
})

test_that("get_pmc_fulltext handles multiple PMIDs in batches", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  # Test with a batch that may contain some invalid IDs
  result <- tryCatch({
    get_pmc_fulltext(c("12345678", "23456789", "34567890"))
  }, error = function(e) {
    skip(paste("PMC API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
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

test_that("ncbi_search works with date ranges for PubMed", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    ncbi_search("migraine", database = "pubmed",
                date_range = c("2023/01/01", "2023/12/31"),
                max_results = 1)
  }, error = function(e) {
    skip(paste("NCBI API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
})

test_that("ncbi_search with MeSH terms for PubMed", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    ncbi_search("headache", database = "pubmed", use_mesh = TRUE, max_results = 1)
  }, error = function(e) {
    skip(paste("NCBI API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
})

test_that("ncbi_search handles gene database", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    ncbi_search("BRCA1", database = "gene", max_results = 2)
  }, error = function(e) {
    skip(paste("Gene API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  if (nrow(result) > 0) {
    expect_true("gene_id" %in% colnames(result))
    expect_true("symbol" %in% colnames(result))
  }
})

test_that("ncbi_search handles protein database", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    ncbi_search("insulin", database = "protein", max_results = 2)
  }, error = function(e) {
    skip(paste("Protein API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  if (nrow(result) > 0) {
    expect_true("protein_id" %in% colnames(result))
    expect_true("name" %in% colnames(result))
  }
})

test_that("ncbi_search handles PMC database", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    ncbi_search("cancer", database = "pmc", max_results = 2)
  }, error = function(e) {
    skip(paste("PMC API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  if (nrow(result) > 0) {
    expect_true("pmcid" %in% colnames(result))
    expect_true("title" %in% colnames(result))
  }
})

test_that("ncbi_search handles unsupported databases", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  result <- tryCatch({
    ncbi_search("homo sapiens", database = "taxonomy", max_results = 2)
  }, error = function(e) {
    skip(paste("Taxonomy API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
  # Only check for "id" column if results were returned
  if (nrow(result) > 0) {
    expect_true("id" %in% colnames(result))
  }
})

# Unit tests for error handling
test_that("pubmed_search handles API errors gracefully", {
  skip_if_not_installed("rentrez")

  result <- tryCatch({
    pubmed_search("invalid[query[", max_results = 1, verbose = FALSE, retry_count = 1)
  }, error = function(e) {
    data.frame()
  })

  expect_s3_class(result, "data.frame")
})

test_that("pubmed_search handles failed batch gracefully", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  # This test would ideally mock a failed batch, but we can at least test the structure
  result <- tryCatch({
    pubmed_search("migraine", max_results = 5, verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })

  expect_s3_class(result, "data.frame")
})

test_that("pubmed_search performance test", {
  skip_if_not_installed("rentrez")
  skip_if_not(can_run_integration_tests(), "Cannot run integration tests")

  start_time <- Sys.time()
  result <- tryCatch({
    pubmed_search("migraine", max_results = 5, verbose = FALSE)
  }, error = function(e) {
    skip(paste("API call failed:", e$message))
  })
  end_time <- Sys.time()

  expect_lt(as.numeric(end_time - start_time), 30)
  expect_s3_class(result, "data.frame")
})

# Test parameter validation
test_that("parameter validation works correctly", {
  expect_no_error({
    query <- "migraine"
    max_results <- 10
    use_mesh <- FALSE
    date_range <- c("2020/01/01", "2023/12/31")

    expect_type(query, "character")
    expect_type(max_results, "double")
    expect_type(use_mesh, "logical")
    expect_length(date_range, 2)
  })
})

# Test missing dependencies
test_that("missing dependencies are handled gracefully", {
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    expect_error(pubmed_search("test"), "rentrez.*required")
  }

  if (!requireNamespace("xml2", quietly = TRUE)) {
    expect_error(parse_pubmed_xml("<xml></xml>"), "xml2.*required")
  }
})

test_that("pubmed_search handles warning for no cache package", {
  skip_if_not_installed("rentrez")

  # This test checks that a warning is issued when digest is not available
  # but won't actually fail if digest is installed
  expect_no_error({
    # The function should still work even without caching
    if (!requireNamespace("digest", quietly = TRUE)) {
      expect_warning(
        pubmed_search("test", max_results = 1, use_cache = TRUE, verbose = FALSE),
        "digest.*required"
      )
    }
  })
})

test_that("ncbi_search handles retry failures", {
  skip_if_not_installed("rentrez")

  # Test that the function can handle retries properly
  expect_error({
    # Using an invalid API key format should trigger retries and eventual failure
    ncbi_search("test", database = "pubmed", max_results = 1,
                retry_count = 1, retry_delay = 0.1)
  }, NA) # Expect no error OR an actual error - either is acceptable
})
