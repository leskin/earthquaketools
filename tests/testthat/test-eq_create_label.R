context("eq_create_label")

## TODO: Add more tests

# capture_output() is useful here

test_that("eq_create_label produces a plot without errors using eq_map", {
  expect_failure(expect_error(readRDS("eq_US_China_clean.rds") %>% dplyr::mutate(popup_text = eq_create_label(.)) %>%
                                eq_map(annot_col = "popup_text")))
})
