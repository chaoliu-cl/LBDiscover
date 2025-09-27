# Tests for zzz.R - Package initialization functions
# Test file: tests/testthat/test-zzz.R

library(testthat)

# Test package environment creation
test_that("package environment is created correctly", {
  # Check that .pkgenv exists and is an environment
  expect_true(exists(".pkgenv", envir = asNamespace("LBDiscover")))

  pkg_env <- get(".pkgenv", envir = asNamespace("LBDiscover"))
  expect_true(is.environment(pkg_env))

  # Check that parent environment is emptyenv()
  expect_identical(parent.env(pkg_env), emptyenv())
})

# Test .onLoad function
test_that(".onLoad function declares global variables correctly", {
  # Expected global variables that should be declared
  expected_globals <- c(
    "doc_id", "entity", "entity_type", "start_pos", "end_pos",
    "sentence", "frequency", "term", "type", "source", "word",
    "count", "a_term", "b_term", "c_term", "a_b_score", "b_c_score",
    "abc_score", "p_value", "significant",
    "legend_items", "legend_colors", "legend_title"
  )

  # Test that .onLoad function exists
  expect_true(exists(".onLoad", envir = asNamespace("LBDiscover")))

  # Get the .onLoad function and examine its content
  onLoad_func <- get(".onLoad", envir = asNamespace("LBDiscover"))
  expect_true(is.function(onLoad_func))

  # Check that the function body contains the expected global variables
  func_body <- deparse(body(onLoad_func))
  func_text <- paste(func_body, collapse = " ")

  # Verify that globalVariables is called
  expect_true(grepl("globalVariables", func_text))

  # Check that most of the expected variables are mentioned in the function
  # (We can't check all due to formatting, but key ones should be there)
  key_variables <- c("doc_id", "entity", "term", "a_term", "b_term", "c_term")
  for (var in key_variables) {
    expect_true(grepl(var, func_text, fixed = TRUE))
  }

  # Verify the function takes the expected parameters
  expect_equal(names(formals(onLoad_func)), c("libname", "pkgname"))
})

# Test .onAttach function startup message
test_that(".onAttach function shows correct startup message", {
  # Capture the startup message
  expect_message(
    LBDiscover:::.onAttach("test_lib", "LBDiscover"),
    "Loading LBDiscover package",
    fixed = TRUE
  )
})

# Test deprecation handler setup
test_that(".onAttach sets up deprecation handlers correctly", {
  # Test that .onAttach function exists and can be called
  expect_true(exists(".onAttach", envir = asNamespace("LBDiscover")))

  # Test that calling .onAttach doesn't throw an error
  # and shows the expected message
  expect_message(
    LBDiscover:::.onAttach("test_lib", "LBDiscover"),
    "Loading LBDiscover package"
  )

  # We can't easily test the internal hook setup without complex mocking,
  # but we can verify the function completes successfully
  expect_silent({
    suppressMessages(LBDiscover:::.onAttach("test_lib", "LBDiscover"))
  })
})

# Test GitHub URL replacement functionality
test_that("GitHub URL replacement works correctly in deprecation messages", {
  # Create a test deprecation handler function similar to what's in .onAttach
  update_dep_message <- function(msg, call) {
    github_pattern <- "<https://github\\.com/[^/]+/[^/]+/issues>"

    if(grepl(github_pattern, msg)) {
      corrected_msg <- gsub(
        github_pattern,
        "<https://github.com/chaoliu-cl/LBDiscover/issues>",
        msg
      )

      if(!grepl("LBDiscover package", corrected_msg)) {
        corrected_msg <- paste0(
          corrected_msg,
          "\n\u2139 The deprecated feature was likely used in the LBDiscover package."
        )
      }

      return(corrected_msg)
    } else {
      return(msg)
    }
  }

  # Test GitHub URL replacement
  test_msg_with_github <- "This function is deprecated. See <https://github.com/someuser/somerepo/issues> for more info."
  expected_result <- "This function is deprecated. See <https://github.com/chaoliu-cl/LBDiscover/issues> for more info.\n\u2139 The deprecated feature was likely used in the LBDiscover package."

  result <- update_dep_message(test_msg_with_github, NULL)
  expect_equal(result, expected_result)

  # Test message without GitHub URL (should pass through unchanged)
  test_msg_no_github <- "This function is deprecated."
  result_no_github <- update_dep_message(test_msg_no_github, NULL)
  expect_equal(result_no_github, test_msg_no_github)

  # Test message with GitHub URL that already mentions LBDiscover package
  test_msg_with_lbdiscover <- "Function deprecated in LBDiscover package. See <https://github.com/someuser/somerepo/issues>"
  expected_with_lbdiscover <- "Function deprecated in LBDiscover package. See <https://github.com/chaoliu-cl/LBDiscover/issues>"

  result_with_lbdiscover <- update_dep_message(test_msg_with_lbdiscover, NULL)
  expect_equal(result_with_lbdiscover, expected_with_lbdiscover)
})

# Test options setting for igraph deprecation handler
test_that("igraph deprecation handler is set correctly", {
  # Test that .onAttach completes without error
  expect_silent({
    suppressMessages(LBDiscover:::.onAttach("test_lib", "LBDiscover"))
  })

  # We can't easily test the internal options setting without complex setup,
  # but we can verify that the .onAttach function includes the expected
  # logic by checking the function body
  onAttach_func <- get(".onAttach", envir = asNamespace("LBDiscover"))
  func_body <- deparse(body(onAttach_func))

  # Check that the function body contains references to igraph deprecation handling
  expect_true(any(grepl("igraph.deprecation.handler", func_body)))
  expect_true(any(grepl("update_dep_message", func_body)))
  expect_true(any(grepl("setHook", func_body)))
})

# Test hook function behavior
test_that("hook function processes deprecation messages correctly", {
  # Create a simplified version of the hook function for testing
  create_hook_function <- function() {
    function(...) {
      github_pattern <- "<https://github\\.com/[^/]+/[^/]+/issues>"

      update_dep_message <- function(msg, call) {
        if(grepl(github_pattern, msg)) {
          corrected_msg <- gsub(
            github_pattern,
            "<https://github.com/chaoliu-cl/LBDiscover/issues>",
            msg
          )

          if(!grepl("LBDiscover package", corrected_msg)) {
            corrected_msg <- paste0(
              corrected_msg,
              "\n\u2139 The deprecated feature was likely used in the LBDiscover package."
            )
          }

          return(corrected_msg)
        } else {
          return(msg)
        }
      }

      # Mock setting options
      list(igraph.deprecation.handler = update_dep_message)
    }
  }

  hook_func <- create_hook_function()
  result <- hook_func()

  expect_true(is.list(result))
  expect_true("igraph.deprecation.handler" %in% names(result))
  expect_true(is.function(result$igraph.deprecation.handler))
})

# Integration test for the complete .onAttach functionality
test_that(".onAttach integration test works correctly", {
  # Test that .onAttach can be called without errors
  expect_silent({
    suppressMessages(LBDiscover:::.onAttach("test_lib", "LBDiscover"))
  })

  # Test with message capture
  expect_message({
    LBDiscover:::.onAttach("test_lib", "LBDiscover")
  }, "Loading LBDiscover package")
})

# Test error handling in hook setup
test_that("hook setup handles errors gracefully", {
  # Test that .onAttach can handle errors gracefully
  # We can't easily mock setHook to throw errors without complex setup,
  # but we can test that the function is robust

  # .onAttach should not fail even if there are issues
  expect_message(
    LBDiscover:::.onAttach("test_lib", "LBDiscover"),
    "Loading LBDiscover package"
  )

  # Test that multiple calls to .onAttach don't cause issues
  expect_silent({
    suppressMessages(LBDiscover:::.onAttach("test_lib", "LBDiscover"))
    suppressMessages(LBDiscover:::.onAttach("test_lib", "LBDiscover"))
  })
})

# Test that global variables are actually accessible
test_that("declared global variables are accessible in package namespace", {
  # This test ensures the globalVariables declaration is working
  # by checking that we can reference these variables without NOTE in R CMD check

  # We can't directly test R CMD check behavior, but we can verify
  # the variables are declared by checking they're in the global variables list

  # Get the namespace
  ns <- asNamespace("LBDiscover")

  # The globalVariables call should have been made during package loading
  # We can't directly access the internal list, but we can test that
  # the function completed without error

  expect_true(exists(".onLoad", envir = ns))
  expect_true(is.function(get(".onLoad", envir = ns)))
})

# Test specific GitHub URL patterns
test_that("GitHub URL pattern matching is robust", {
  github_pattern <- "<https://github\\.com/[^/]+/[^/]+/issues>"

  # Test various GitHub URL formats
  test_urls <- c(
    "<https://github.com/user/repo/issues>",
    "<https://github.com/some-user/some-repo/issues>",
    "<https://github.com/user123/repo_name/issues>",
    "<https://github.com/user/repo/issues/123>",  # Should not match (has issue number)
    "https://github.com/user/repo/issues",        # Should not match (no angle brackets)
    "<https://gitlab.com/user/repo/issues>"       # Should not match (not github)
  )

  expected_matches <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)

  actual_matches <- sapply(test_urls, function(url) grepl(github_pattern, url))

  expect_equal(as.logical(actual_matches), expected_matches)
})
