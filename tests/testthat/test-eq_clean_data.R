context("eq_clean_data")

## TODO: Add more tests

test_that("eq_clean_data produces a data.frame given raw earthquake data frame as input", {
  expect_message(eq_clean_data(readr::read_delim("earthquakes.tsv.gz", delim = "\t")))
})
