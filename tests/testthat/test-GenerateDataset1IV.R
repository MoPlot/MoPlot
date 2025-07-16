library(testthat)

test_that("Defaults", {
  sample_size <- round(stats::runif(1,10,100))
  means_groups <- stats::runif(3,100,200)
  sd <- round(stats::runif(1,20,50))
  df <- GenerateDataset1IV(n_sample = sample_size, means = c(0, 1, 2), sds = 1)
  expect_equal(nrow(df), sample_size * length(means_groups))
})


