library(testthat)

test_that("preprocess_text handles various text formats", {
  # Test with empty strings
  empty_data <- data.frame(
    doc_id = 1:3,
    abstract = c("", NA, "   "),
    stringsAsFactors = FALSE
  )

  result <- preprocess_text(empty_data, text_column = "abstract")
  expect_true(nrow(result) <= 3)  # Some rows may be filtered out

  # Test with special characters
  special_data <- data.frame(
    doc_id = 1:2,
    abstract = c("Test with émojis 😀 and symbols #@$%",
                 "Numbers 123 and punctuation... work?"),
    stringsAsFactors = FALSE
  )

  result <- preprocess_text(special_data, text_column = "abstract")
  expect_true(is.data.frame(result))
  expect_true("terms" %in% colnames(result))
})

test_that("sanitize_dictionary removes problematic terms", {
  dirty_dict <- data.frame(
    term = c("valid_term", "", NA, "123", "europe", "optimization"),
    type = c("disease", "empty", "missing", "number", "location", "process"),
    stringsAsFactors = FALSE
  )

  clean_dict <- sanitize_dictionary(dirty_dict, verbose = FALSE)

  # Should remove empty, NA, and numeric-only terms at minimum
  expect_true(nrow(clean_dict) < nrow(dirty_dict))
  expect_false("" %in% clean_dict$term)
  expect_false("123" %in% clean_dict$term)

  # Check that remaining terms are non-empty
  expect_true(all(nchar(clean_dict$term) > 0))
  expect_true(all(!is.na(clean_dict$term)))
})

test_that("extract_entities handles overlapping terms", {
  text_data <- data.frame(
    doc_id = 1,
    abstract = "migraine headache and severe headache pain",
    stringsAsFactors = FALSE
  )

  dictionary <- data.frame(
    term = c("migraine", "headache", "severe headache", "pain"),
    type = c("disease", "symptom", "symptom", "symptom"),
    stringsAsFactors = FALSE
  )

  # Test different overlap strategies
  result_priority <- extract_entities(text_data, dictionary = dictionary,
                                      overlap_strategy = "priority")
  result_longest <- extract_entities(text_data, dictionary = dictionary,
                                     overlap_strategy = "longest")
  result_all <- extract_entities(text_data, dictionary = dictionary,
                                 overlap_strategy = "all")

  expect_true(nrow(result_all) >= nrow(result_priority))
  expect_true(nrow(result_all) >= nrow(result_longest))
})
