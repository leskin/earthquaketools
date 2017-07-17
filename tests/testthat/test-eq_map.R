context("eq_map")

## TODO: Add more tests

# capture_output() is useful here

test_that("eq_map produces a plot without errors", {
  expect_failure(expect_error(eq_map(readRDS("eq_US_China_clean.rds"), annot_col = "DATE")))
})
