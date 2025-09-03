test_that("format_table returns correct structure", {
  # Generate test data
  sim_data <- do_simulation(N_sim = 5, seed = 123, data_N = 50)
  stats_data <- analyse_simulations(sim_data)
  
  # Test basic functionality
  result <- format_table(stats_data$shared_results)
  
  expect_s3_class(result, "data.frame")
  expect_true("Section" %in% names(result))
  expect_true("Statistic" %in% names(result))
  expect_true("MAIC" %in% names(result))
  expect_true("STC" %in% names(result))
  expect_true("Bucher" %in% names(result))
})

test_that("format_table handles decimal places parameter", {
  sim_data <- do_simulation(N_sim = 5, seed = 123, data_N = 50)
  stats_data <- analyse_simulations(sim_data)
  
  # Test different decimal places
  result_3dp <- format_table(stats_data$shared_results, dp = 3)
  result_2dp <- format_table(stats_data$shared_results, dp = 2)
  result_1dp <- format_table(stats_data$shared_results, dp = 1)
  
  expect_s3_class(result_3dp, "data.frame")
  expect_s3_class(result_2dp, "data.frame")
  expect_s3_class(result_1dp, "data.frame")
  
  # All should have same structure
  expect_equal(dim(result_3dp), dim(result_2dp))
  expect_equal(dim(result_2dp), dim(result_1dp))
})

test_that("format_table produces expected sections", {
  sim_data <- do_simulation(N_sim = 5, seed = 123, data_N = 50)
  stats_data <- analyse_simulations(sim_data)
  result <- format_table(stats_data$shared_results)
  
  # Check that expected sections are present
  sections <- unique(result$Section[result$Section != ""])
  expected_sections <- c("Bias", "Empirical SE", "Model SE", "MSE", "Coverage")
  expect_true(all(expected_sections %in% sections))
})

test_that("format_table handles edge cases", {
  sim_data <- do_simulation(N_sim = 3, seed = 123, data_N = 30)
  stats_data <- analyse_simulations(sim_data)
  
  # Should not error with minimal data
  expect_no_error(format_table(stats_data$shared_results))
  
  # Should handle extreme decimal places
  expect_no_error(format_table(stats_data$shared_results, dp = 0))
  expect_no_error(format_table(stats_data$shared_results, dp = 10))
})
