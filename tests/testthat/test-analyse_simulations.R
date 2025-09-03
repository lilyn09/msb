test_that("analyse_simulations returns correct structure", {
  # Generate simulation data for testing
  sim_data <- do_simulation(N_sim = 10, seed = 123, data_N = 50)
  
  # Test basic functionality
  result <- analyse_simulations(sim_data)
  
  expect_type(result, "list")
  expect_true("shared_results" %in% names(result))
  expect_s3_class(result$shared_results, "data.frame")
  expect_equal(nrow(result$shared_results), 1)
})

test_that("analyse_simulations contains expected statistics", {
  sim_data <- do_simulation(N_sim = 10, seed = 123, data_N = 50)
  result <- analyse_simulations(sim_data)
  
  # Check for key statistical measures
  expected_cols <- c("maic_bias", "maic_empSE", "maic_modSE", "maic_coverage",
                     "stc_bias", "stc_empSE", "stc_modSE", "stc_coverage",
                     "bucher_bias", "bucher_empSE", "bucher_modSE", "bucher_coverage")
  
  expect_true(all(expected_cols %in% names(result$shared_results)))
  
  # Check that all values are numeric
  for (col in expected_cols) {
    expect_type(result$shared_results[[col]], "double")
  }
})

test_that("analyse_simulations handles edge cases", {
  # Test with minimal simulation data
  sim_data_small <- do_simulation(N_sim = 3, seed = 123, data_N = 30)
  expect_no_error(analyse_simulations(sim_data_small))
  
  # Test reproducibility
  sim_data1 <- do_simulation(N_sim = 5, seed = 456, data_N = 50)
  sim_data2 <- do_simulation(N_sim = 5, seed = 456, data_N = 50)
  result1 <- analyse_simulations(sim_data1)
  result2 <- analyse_simulations(sim_data2)
  
  expect_equal(result1, result2)
})
