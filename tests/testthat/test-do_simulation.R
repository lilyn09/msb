test_that("do_simulation returns correct structure", {
  # Test with small number of simulations for speed
  result <- do_simulation(N_sim = 5, seed = 123, data_N = 50)
  
  expect_type(result, "list")
  expect_named(result, c("results_df", "true_val"))
  
  # Test results_df structure
  expect_s3_class(result$results_df, "data.frame")
  expect_equal(nrow(result$results_df), 5)
  
  # Check required columns exist
  expected_cols <- c("maic_log_OR", "maic_se", "maic_ci_lower", "maic_ci_upper",
                     "stc_log_OR", "stc_se", "stc_ci_lower", "stc_ci_upper",
                     "bucher_log_OR", "bucher_se", "bucher_ci_lower", "bucher_ci_upper")
  expect_true(all(expected_cols %in% names(result$results_df)))
  
  # Test true_val
  expect_type(result$true_val, "double")
  expect_length(result$true_val, 1)
})

test_that("do_simulation parameters work correctly", {
  # Test different N_sim
  result_small <- do_simulation(N_sim = 2, seed = 123, data_N = 50)
  expect_equal(nrow(result_small$results_df), 2)
  
  # Test reproducibility with same seed
  result1 <- do_simulation(N_sim = 3, seed = 456, data_N = 50)
  result2 <- do_simulation(N_sim = 3, seed = 456, data_N = 50)
  expect_equal(result1$results_df, result2$results_df)
  expect_equal(result1$true_val, result2$true_val)
})

test_that("do_simulation handles different weight types", {
  # Test rescaled_weights
  result_rescaled <- do_simulation(N_sim = 2, seed = 123, data_N = 50, 
                                   wts_type = "rescaled_weights")
  expect_s3_class(result_rescaled$results_df, "data.frame")
  
  # Test weights
  result_weights <- do_simulation(N_sim = 2, seed = 123, data_N = 50, 
                                  wts_type = "weights")
  expect_s3_class(result_weights$results_df, "data.frame")
})

test_that("do_simulation handles different parameters", {
  # Test different data parameters
  expect_no_error(do_simulation(N_sim = 2, data_N = 100, data_em_strength = 0))
  expect_no_error(do_simulation(N_sim = 2, data_N = 100, data_overlap_param = 0))
  expect_no_error(do_simulation(N_sim = 2, data_N = 100, data_adjust_all = FALSE))
})
