test_that("gen_data returns correct structure", {
  # Test basic functionality
  data <- gen_data(N = 100, seed = 123)
  
  expect_type(data, "list")
  expect_named(data, c("AB_IPD", "AC_aggregate"))
  
  # Test AB_IPD structure
  expect_s3_class(data$AB_IPD, "data.frame")
  expect_equal(nrow(data$AB_IPD), 100)
  expect_named(data$AB_IPD, c("X1", "X2", "treatment", "y", "y0"))
  expect_equal(length(unique(data$AB_IPD$treatment)), 2)
  expect_true(all(data$AB_IPD$treatment %in% c("A", "B")))
  
  # Test AC_aggregate structure
  expect_s3_class(data$AC_aggregate, "data.frame")
  expect_equal(nrow(data$AC_aggregate), 1)
  expect_true(all(c("N_A", "N_C", "X1_mean", "X1_sd", "X2_mean", "X2_sd", 
                    "y_A_sum", "y_C_sum") %in% names(data$AC_aggregate)))
})

test_that("gen_data parameters work correctly", {
  # Test different sample sizes
  data_small <- gen_data(N = 50, seed = 123)
  expect_equal(nrow(data_small$AB_IPD), 50)
  
  # Test reproducibility with same seed
  data1 <- gen_data(N = 100, seed = 456)
  data2 <- gen_data(N = 100, seed = 456)
  expect_equal(data1$AB_IPD$X1, data2$AB_IPD$X1)
  expect_equal(data1$AC_aggregate, data2$AC_aggregate)
})

test_that("gen_data handles edge cases", {
  # Test very small sample size
  expect_no_error(gen_data(N = 10))
  
  # Test different parameter values
  expect_no_error(gen_data(em_strength = 0))
  expect_no_error(gen_data(overlap_param = 0))
  expect_no_error(gen_data(overlap_param = 1))
})
