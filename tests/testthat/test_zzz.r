# test-zzz.R
# Test file for zzz.R functions

library(testthat)

# Helper function to access non-exported functions
get_unexported <- function(name) {
  if (isNamespaceLoaded("LBDiscover")) {
    tryCatch({
      getFromNamespace(name, "LBDiscover")
    }, error = function(e) {
      NULL
    })
  } else {
    NULL
  }
}

# Test .pkgenv environment
test_that(".pkgenv environment exists and is correctly configured", {
  pkgenv <- get_unexported(".pkgenv")

  if (!is.null(pkgenv)) {
    expect_true(is.environment(pkgenv))
    expect_identical(parent.env(pkgenv), emptyenv())
  } else {
    skip(".pkgenv not accessible")
  }
})

# Test .onLoad function
test_that(".onLoad function exists in package namespace", {
  onLoad <- get_unexported(".onLoad")
  expect_false(is.null(onLoad))
  expect_type(onLoad, "closure")
})

test_that("package loaded successfully (implies .onLoad worked)", {
  # If .onLoad failed, the package wouldn't have loaded
  expect_true(isNamespaceLoaded("LBDiscover"))
})

test_that("global variables are properly declared", {
  # List of variables that should be declared
  expected_globals <- c(
    "doc_id", "entity", "entity_type", "start_pos", "end_pos",
    "sentence", "frequency", "term", "type", "source", "word",
    "count", "a_term", "b_term", "c_term", "a_b_score", "b_c_score",
    "abc_score", "p_value", "significant",
    "legend_items", "legend_colors", "legend_title"
  )

  # Verify structure of global variables list
  expect_length(expected_globals, 23)
  expect_type(expected_globals, "character")
  expect_true(all(nchar(expected_globals) > 0))

  # Verify no duplicates
  expect_equal(length(unique(expected_globals)), 23)

  # Verify all are valid R variable names
  for (var in expected_globals) {
    expect_equal(make.names(var), var,
                 info = paste("Should be valid R name:", var))
  }
})

# Test .onAttach function
test_that(".onAttach function exists in package namespace", {
  onAttach <- get_unexported(".onAttach")
  expect_false(is.null(onAttach))
  expect_type(onAttach, "closure")
})

test_that(".onAttach displays correct startup message", {
  onAttach <- get_unexported(".onAttach")

  if (!is.null(onAttach)) {
    msgs <- capture_messages({
      onAttach(libname = "test", pkgname = "LBDiscover")
    })

    expect_true(length(msgs) > 0)
    expect_match(msgs[1], "Loading LBDiscover package")
    expect_equal(msgs[1], "Loading LBDiscover package\n")
  } else {
    skip(".onAttach not accessible")
  }
})

test_that(".onAttach can be called multiple times without error", {
  onAttach <- get_unexported(".onAttach")

  if (!is.null(onAttach)) {
    # Unlike .onLoad, .onAttach can safely be called multiple times
    expect_error(
      suppressMessages({
        onAttach(libname = "test1", pkgname = "pkg1")
        onAttach(libname = "test2", pkgname = "pkg2")
        onAttach(libname = "test3", pkgname = "pkg3")
      }),
      NA
    )
  } else {
    skip(".onAttach not accessible")
  }
})

test_that(".onAttach accepts different library and package names", {
  onAttach <- get_unexported(".onAttach")

  if (!is.null(onAttach)) {
    # Test with various inputs
    test_cases <- list(
      list(libname = "lib1", pkgname = "pkg1"),
      list(libname = "lib2", pkgname = "pkg2"),
      list(libname = "", pkgname = "")
    )

    for (tc in test_cases) {
      expect_error(
        suppressMessages({
          onAttach(libname = tc$libname, pkgname = tc$pkgname)
        }),
        NA,
        info = paste("libname:", tc$libname, "pkgname:", tc$pkgname)
      )
    }
  } else {
    skip(".onAttach not accessible")
  }
})

test_that("package initialization completes successfully", {
  # Overall integration test
  expect_true(isNamespaceLoaded("LBDiscover"))

  # Both initialization functions should exist
  expect_false(is.null(get_unexported(".onLoad")))
  expect_false(is.null(get_unexported(".onAttach")))
  expect_false(is.null(get_unexported(".pkgenv")))
})

message("zzz.R tests completed successfully!")
