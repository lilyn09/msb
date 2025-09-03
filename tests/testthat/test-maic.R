test_that("maic returns correct structure", {
  # Generate test data
  data <- gen_data(N = 100, seed = 123)
  
  # Test basic MAIC functionality
  result <- maic(data$AB_IPD, data$AC_aggregate)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c("maic_log_OR", "maic_ESS", "maic_se", "maic_ci_lower", 
                    "maic_ci_upper") %in% names(result)))
  
  # Check that results are numeric
  expect_type(result$maic_log_OR, "double")
  expect_type(result$maic_ESS, "double")
  expect_type(result$maic_se, "double")
  expect_type(result$maic_ci_lower, "double")
  expect_type(result$maic_ci_upper, "double")
})

test_that("maic parameters work correctly", {
  data <- gen_data(N = 100, seed = 123)
  
  # Test adjust_all parameter
  result_all <- maic(data$AB_IPD, data$AC_aggregate, adjust_all = TRUE)
  result_partial <- maic(data$AB_IPD, data$AC_aggregate, adjust_all = FALSE)
  
  expect_s3_class(result_all, "data.frame")
  expect_s3_class(result_partial, "data.frame")
  
  # Test type parameter
  result_rescaled <- maic(data$AB_IPD, data$AC_aggregate, type = "rescaled_weights")
  result_weights <- maic(data$AB_IPD, data$AC_aggregate, type = "weights")
  
  expect_s3_class(result_rescaled, "data.frame")
  expect_s3_class(result_weights, "data.frame")
})

test_that("maic handles edge cases", {
  data <- gen_data(N = 50, seed = 123)
  
  # Should not error with small sample sizes
  expect_no_error(maic(data$AB_IPD, data$AC_aggregate))
  
  # Test with extreme overlap parameters
  data_no_overlap <- gen_data(N = 100, overlap_param = 0, seed = 123)
  expect_no_error(maic(data_no_overlap$AB_IPD, data_no_overlap$AC_aggregate))
})
