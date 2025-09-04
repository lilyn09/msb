 data <- gen_data(N = 50, em_strength = 0.25, overlap_param = 0.75, 
 seed = 1234)

test_that("gen_data returns correct structure", {
  expect_true(is.list(data))
  expect_named(data, c("AB_IPD", "AC_aggregate"))
  
  # Test AB_IPD structure
  expect_s3_class(data$AB_IPD, "data.frame")
  expect_equal(nrow(data$AB_IPD), 50)
  expect_equal(ncol(data$AB_IPD), 4)
  expect_named(data$AB_IPD, c("X1", "X2", "treatment", "y"))
  expect_equal(length(unique(data$AB_IPD$treatment)), 2)
  expect_true(all(data$AB_IPD$treatment %in% c("A", "B")))
  expect_true(all(data$AB_IPD$y %in% c(0, 1)))
  
  # Test AC_aggregate structure
  expect_s3_class(data$AC_aggregate, "data.frame")
  expect_equal(nrow(data$AC_aggregate), 1)
  expect_equal(ncol(data$AC_aggregate), 10)
  expected_cols <- c("X1_mean", "X1_sd", "X2_mean", "X2_sd", "y_A_sum", "y_A_mean", "N_A", "y_C_sum", "y_C_mean", "N_C")
  expect_named(data$AC_aggregate, expected_cols)
})

test_that("test reproducibility with same seed", {
  data1 <- gen_data(N = 100, seed = 456)
  data2 <- gen_data(N = 100, seed = 456)
  expect_equal(data1$AB_IPD$X1, data2$AB_IPD$X1)
  expect_equal(data1$AC_aggregate, data2$AC_aggregate)
})


