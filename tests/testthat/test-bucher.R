test_that("bucher returns correct structure", {
  # Generate test data
  data <- gen_data(N = 100, seed = 123)
  
  # Test basic Bucher functionality
  result <- bucher(data$AB_IPD, data$AC_aggregate)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c("bucher_log_OR", "bucher_se", "bucher_ci_lower", 
                    "bucher_ci_upper") %in% names(result)))
  
  # Check that results are numeric
  expect_type(result$bucher_log_OR, "double")
  expect_type(result$bucher_se, "double")
  expect_type(result$bucher_ci_lower, "double")
  expect_type(result$bucher_ci_upper, "double")
})

test_that("bucher produces reasonable results", {
  data <- gen_data(N = 200, seed = 123)
  result <- bucher(data$AB_IPD, data$AC_aggregate)
  
  # CI should be ordered correctly
  expect_true(result$bucher_ci_lower < result$bucher_ci_upper)
  
  # Standard error should be positive
  expect_true(result$bucher_se > 0)
})

test_that("bucher handles different data scenarios", {
  # Test with different sample sizes
  data_small <- gen_data(N = 50, seed = 123)
  expect_no_error(bucher(data_small$AB_IPD, data_small$AC_aggregate))
  
  data_large <- gen_data(N = 500, seed = 123)
  expect_no_error(bucher(data_large$AB_IPD, data_large$AC_aggregate))
  
  # Test reproducibility
  data1 <- gen_data(N = 100, seed = 456)
  data2 <- gen_data(N = 100, seed = 456)
  result1 <- bucher(data1$AB_IPD, data1$AC_aggregate)
  result2 <- bucher(data2$AB_IPD, data2$AC_aggregate)
  
  expect_equal(result1, result2)
})
