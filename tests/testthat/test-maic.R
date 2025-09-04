data <- gen_data(N = 100, seed = 123)

test_that("maic returns correct structure", {
  result <- maic(data$AB_IPD, data$AC_aggregate)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  expected_cols <- c("maic_log_OR", "maic_se", "N", "ESS", "ESS_N_ratio", 
  "Min", "Q1", "Median", "Q3", "Max", "Mean", "SD", 
  "zero_weights", "greater_than_3_weights", 
  "maic_ci_lower", "maic_ci_upper")
  expect_equal(ncol(result), length(expected_cols))
  expect_true(all(colnames(result) %in% expected_cols))
})



