test_that("download_stock('SPY', 'yahoo') at 2022-03-01", {
  spy <- download_stock("SPY", "yahoo")
  close_20220301 <- spy %>%
    dplyr::filter(date == as.Date("2022-03-01")) %>%
    dplyr::pull(close)
  expect_equal(close_20220301, 429.98, tolerance = 0.01)
})
