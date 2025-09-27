library(testthat)

test_that("pubmed_search handles API failures gracefully", {
  skip_if_not(requireNamespace("rentrez", quietly = TRUE))

  # Test with invalid query that should return no results
  result <- pubmed_search("xyzinvalidqueryterm12345", max_results = 10)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("parse_pubmed_xml handles malformed XML", {
  # Test with empty XML
  empty_xml <- "<root></root>"
  result <- parse_pubmed_xml(empty_xml)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)

  # Test with invalid XML
  expect_error(parse_pubmed_xml("not valid xml"))
})

test_that("retry_api_call implements backoff correctly", {
  call_count <- 0
  failing_function <- function() {
    call_count <<- call_count + 1
    stop("Simulated API failure")
  }

  result <- retry_api_call(failing_function, retry_count = 3, verbose = FALSE)
  expect_null(result)
  expect_equal(call_count, 3)
})
