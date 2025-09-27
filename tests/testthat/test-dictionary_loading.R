library(testthat)

test_that("load_dictionary handles missing sources gracefully", {
  # Test loading with invalid source
  expect_error(load_dictionary("disease", source = "invalid_source"))

  # Test loading unsupported type for local source
  result <- load_dictionary("unsupported_type", source = "local")
  expect_true(is.data.frame(result))
  # Should fallback to mesh or return dummy dictionary
})

test_that("dictionary caching works correctly", {
  # Clear cache first
  clear_dict_cache <- function() {
    cache_env <- get_dict_cache()
    rm(list = ls(envir = cache_env), envir = cache_env)
  }

  clear_dict_cache()

  # Load dictionary twice - second should be from cache
  dict1 <- load_dictionary("disease", source = "local")
  dict2 <- load_dictionary("disease", source = "local")

  expect_equal(dict1, dict2)
})

test_that("create_dummy_dictionary creates valid structure", {
  dummy <- create_dummy_dictionary("disease")

  expect_true(is.data.frame(dummy))
  expect_true(all(c("term", "type", "source") %in% colnames(dummy)))
  expect_true(all(dummy$type == "disease"))
})
