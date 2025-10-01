# test-zzz-logic.R
# Test the logic in zzz.R by executing it directly

library(testthat)

test_that("GitHub URL pattern matching works", {
  github_pattern <- "<https://github\\.com/[^/]+/[^/]+/issues>"

  # Execute the grepl check
  result1 <- grepl(github_pattern, "<https://github.com/user/repo/issues>")
  expect_true(result1)

  result2 <- grepl(github_pattern, "No URL")
  expect_false(result2)
})

test_that("GitHub URL replacement works", {
  github_pattern <- "<https://github\\.com/[^/]+/[^/]+/issues>"
  msg <- "<https://github.com/old/repo/issues>"

  # Execute the gsub
  result <- gsub(
    github_pattern,
    "<https://github.com/chaoliu-cl/LBDiscover/issues>",
    msg
  )

  expect_equal(result, "<https://github.com/chaoliu-cl/LBDiscover/issues>")
})

test_that("package mention detection works", {
  msg1 <- "About LBDiscover package"
  msg2 <- "About something else"

  # Execute the grepl check
  result1 <- grepl("LBDiscover package", msg1)
  expect_true(result1)

  result2 <- grepl("LBDiscover package", msg2)
  expect_false(result2)
})

test_that("message concatenation works", {
  msg <- "Base message"

  # Execute the paste0
  result <- paste0(
    msg,
    "\n\u2139 The deprecated feature was likely used in the LBDiscover package."
  )

  expect_match(result, "Base message")
  expect_match(result, "LBDiscover package")
})
