context("geom_timeline")

## TODO: Add more tests

# capture_output() is useful here

test_that("geom_timeline produces a plot without errors", {
  expect_failure(expect_error(ggplot(readRDS("eq_US_China_clean.rds"),ggplot2::aes(DATE))+geom_timeline(ggplot2::aes(colour = TOTAL_DEATHS, size = EQ_PRIMARY),xmindate=2000,xmaxdate=2017)))
})
