test_that("stc returns correct structure", {
  # Generate test data
  data <- gen_data(N = 100, seed = 123)
  
  # Test basic STC functionality
  result <- stc(data$AB_IPD, data$AC_aggregate)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c("stc_log_OR", "stc_se", "stc_ci_lower", 
                    "stc_ci_upper") %in% names(result)))
  
  # Check that results are numeric
  expect_type(result$stc_log_OR, "double")
  expect_type(result$stc_se, "double")
  expect_type(result$stc_ci_lower, "double")
  expect_type(result$stc_ci_upper, "double")
})

test_that("stc produces reasonable results", {
  data <- gen_data(N = 200, seed = 123)
  result <- stc(data$AB_IPD, data$AC_aggregate)
  
  # CI should be ordered correctly
  expect_true(result$stc_ci_lower < result$stc_ci_upper)
  
  # Standard error should be positive
  expect_true(result$stc_se > 0)
})

test_that("stc handles different data scenarios", {
  # Test with different sample sizes
  data_small <- gen_data(N = 50, seed = 123)
  expect_no_error(stc(data_small$AB_IPD, data_small$AC_aggregate))
  
  data_large <- gen_data(N = 500, seed = 123)
  expect_no_error(stc(data_large$AB_IPD, data_large$AC_aggregate))
  
  # Test with different overlap
  data_no_overlap <- gen_data(N = 100, overlap_param = 0, seed = 123)
  expect_no_error(stc(data_no_overlap$AB_IPD, data_no_overlap$AC_aggregate))
})
