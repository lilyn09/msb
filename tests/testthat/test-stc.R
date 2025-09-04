data <- gen_data(N = 100, seed = 123)

test_that("stc returns correct structure", {
  result <- stc(data$AB_IPD, data$AC_aggregate)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  expected_cols <- c("stc_log_OR", "stc_se", "stc_ci_lower", "stc_ci_upper")
  expect_equal(ncol(result), length(expected_cols))
  expect_named(result, expected_cols)
})


