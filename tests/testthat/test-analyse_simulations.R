sim_data <- do_simulation(N_sim = 10, seed = 123, data_N = 50)
result <- analyse_simulations(sim_data)

test_that("analyse_simulations returns correct structure", {
  expect_type(result, "list")
  expect_true(all(c("shared_results", "maic_wts_results") %in% names(result)))
  expect_s3_class(result$shared_results, "data.frame")
  expect_s3_class(result$maic_wts_results, "data.frame")
  expect_equal(nrow(result$shared_results), 1)
  expect_equal(ncol(result$shared_results), 33)
  expect_equal(nrow(result$maic_wts_results), 12)
  expect_equal(ncol(result$maic_wts_results), 1)
})

test_that("analyse_simulations contains expected statistics", {
  expected_cols <- c(
    "maic_abs_bias", "maic_bias", "maic_bias_mcse", "maic_bias_pct",
    "maic_empSE", "maic_empSE_mcse", "maic_modSE", "maic_modSE_mcse",
    "maic_MSE", "maic_coverage", "maic_coverage_mcse",
    "stc_abs_bias", "stc_bias", "stc_bias_mcse", "stc_bias_pct",
    "stc_empSE", "stc_empSE_mcse", "stc_modSE", "stc_modSE_mcse",
    "stc_MSE", "stc_coverage", "stc_coverage_mcse",
    "bucher_abs_bias", "bucher_bias", "bucher_bias_mcse", "bucher_bias_pct",
    "bucher_empSE", "bucher_empSE_mcse", "bucher_modSE", "bucher_modSE_mcse",
    "bucher_MSE", "bucher_coverage", "bucher_coverage_mcse"
  )
  expect_true(all(expected_cols %in% names(result$shared_results)))
  
  expected_maic_wts_names <- c(
    "N", "ESS", "ESS_N_ratio", "Min", "Q1", "Median", 
    "Q3", "Max", "Mean", "SD", "zero_weights", "greater_than_three_weights"
  )
  expect_true(all(expected_maic_wts_names %in% rownames(result$maic_wts_results)))
})

# Test results are correct
sim_data_small <- do_simulation(N_sim = 5, seed = 42, data_N = 30)
result_small <- analyse_simulations(sim_data_small)
results_df <- sim_data_small$results_df
true_val <- sim_data_small$true_val

test_that("analyse_simulations produces correct bias calculations", {
  # Test MAIC bias calculations
  expected_maic_bias <- mean(results_df$maic_log_OR - true_val, na.rm = TRUE)
  expect_equal(result_small$shared_results$maic_bias, expected_maic_bias, tolerance = 1e-12)
  
  expected_maic_abs_bias <- mean(abs(results_df$maic_log_OR - true_val), na.rm = TRUE)
  expect_equal(result_small$shared_results$maic_abs_bias, expected_maic_abs_bias, tolerance = 1e-12)
  
  expected_maic_bias_pct <- mean((results_df$maic_log_OR - true_val) / true_val * 100, na.rm = TRUE)
  expect_equal(result_small$shared_results$maic_bias_pct, expected_maic_bias_pct, tolerance = 1e-12)
  
  # Test STC bias calculations
  expected_stc_bias <- mean(results_df$stc_log_OR - true_val, na.rm = TRUE)
  expect_equal(result_small$shared_results$stc_bias, expected_stc_bias, tolerance = 1e-12)
  
  expected_stc_abs_bias <- mean(abs(results_df$stc_log_OR - true_val), na.rm = TRUE)
  expect_equal(result_small$shared_results$stc_abs_bias, expected_stc_abs_bias, tolerance = 1e-12)
  
  # Test Bucher bias calculations
  expected_bucher_bias <- mean(results_df$bucher_log_OR - true_val, na.rm = TRUE)
  expect_equal(result_small$shared_results$bucher_bias, expected_bucher_bias, tolerance = 1e-12)
  
  expected_bucher_abs_bias <- mean(abs(results_df$bucher_log_OR - true_val), na.rm = TRUE)
  expect_equal(result_small$shared_results$bucher_abs_bias, expected_bucher_abs_bias, tolerance = 1e-12)
})

test_that("analyse_simulations produces correct standard error calculations", {
  # Test empirical standard errors
  expected_maic_empSE <- sd(results_df$maic_log_OR, na.rm = TRUE)
  expect_equal(result_small$shared_results$maic_empSE, expected_maic_empSE, tolerance = 1e-12)
  
  expected_stc_empSE <- sd(results_df$stc_log_OR, na.rm = TRUE)
  expect_equal(result_small$shared_results$stc_empSE, expected_stc_empSE, tolerance = 1e-12)
  
  expected_bucher_empSE <- sd(results_df$bucher_log_OR, na.rm = TRUE)
  expect_equal(result_small$shared_results$bucher_empSE, expected_bucher_empSE, tolerance = 1e-12)
  
  # Test model-based standard errors
  expected_maic_modSE <- sqrt(mean(results_df$maic_se^2, na.rm = TRUE))
  expect_equal(result_small$shared_results$maic_modSE, expected_maic_modSE, tolerance = 1e-12)
  
  expected_stc_modSE <- sqrt(mean(results_df$stc_se^2, na.rm = TRUE))
  expect_equal(result_small$shared_results$stc_modSE, expected_stc_modSE, tolerance = 1e-12)
  
  expected_bucher_modSE <- sqrt(mean(results_df$bucher_se^2, na.rm = TRUE))
  expect_equal(result_small$shared_results$bucher_modSE, expected_bucher_modSE, tolerance = 1e-12)
  
  # Test MCSE calculations for empirical SE
  n_valid_maic <- sum(!is.na(results_df$maic_log_OR))
  if(n_valid_maic > 1) {
    expected_maic_empSE_mcse <- sd(results_df$maic_log_OR, na.rm = TRUE) / sqrt(2 * (n_valid_maic - 1))
    expect_equal(result_small$shared_results$maic_empSE_mcse, expected_maic_empSE_mcse, tolerance = 1e-12)
  }
})

test_that("analyse_simulations produces correct MSE and coverage calculations", {
  # Test MSE calculations
  expected_maic_mse <- mean((results_df$maic_log_OR - true_val)^2, na.rm = TRUE)
  expect_equal(result_small$shared_results$maic_MSE, expected_maic_mse, tolerance = 1e-12)
  
  expected_stc_mse <- mean((results_df$stc_log_OR - true_val)^2, na.rm = TRUE)
  expect_equal(result_small$shared_results$stc_MSE, expected_stc_mse, tolerance = 1e-12)
  
  expected_bucher_mse <- mean((results_df$bucher_log_OR - true_val)^2, na.rm = TRUE)
  expect_equal(result_small$shared_results$bucher_MSE, expected_bucher_mse, tolerance = 1e-12)
  
  # Test coverage calculations
  expected_maic_coverage <- mean(results_df$maic_ci_lower <= true_val & results_df$maic_ci_upper >= true_val, na.rm = TRUE)
  expect_equal(result_small$shared_results$maic_coverage, expected_maic_coverage, tolerance = 1e-12)
  
  expected_stc_coverage <- mean(results_df$stc_ci_lower <= true_val & results_df$stc_ci_upper >= true_val, na.rm = TRUE)
  expect_equal(result_small$shared_results$stc_coverage, expected_stc_coverage, tolerance = 1e-12)
  
  expected_bucher_coverage <- mean(results_df$bucher_ci_lower <= true_val & results_df$bucher_ci_upper >= true_val, na.rm = TRUE)
  expect_equal(result_small$shared_results$bucher_coverage, expected_bucher_coverage, tolerance = 1e-12)
})

test_that("analyse_simulations produces correct MAIC weights results", {
  # Test each MAIC weights statistic
  weight_cols <- c("N", "ESS", "ESS_N_ratio", "Min", "Q1", "Median", "Q3", "Max", "Mean", "SD", "zero_weights", "greater_than_three_weights")
  
  for(col in weight_cols) {
    expected_mean <- mean(results_df[[col]], na.rm = TRUE)
    actual_value <- result_small$maic_wts_results[col, 1]
    expect_equal(actual_value, expected_mean, tolerance = 1e-12)
  }
})