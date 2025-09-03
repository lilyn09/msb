test_that("facet_coverage_plot returns ggplot object", {
  # Generate test data
  sim_data <- do_simulation(N_sim = 10, seed = 123, data_N = 50)
  
  # Test basic functionality
  plot <- facet_coverage_plot(sim_data$results_df, true_val = sim_data$true_val)
  
  expect_s3_class(plot, "ggplot")
})

test_that("facet_coverage_plot handles different parameters", {
  sim_data <- do_simulation(N_sim = 10, seed = 123, data_N = 50)
  
  # Test with different true_val
  expect_no_error(facet_coverage_plot(sim_data$results_df, true_val = 0))
  expect_no_error(facet_coverage_plot(sim_data$results_df, true_val = -1))
  expect_no_error(facet_coverage_plot(sim_data$results_df, true_val = 1))
})

test_that("facet_coverage_plot handles edge cases", {
  # Test with minimal data
  sim_data_small <- do_simulation(N_sim = 3, seed = 123, data_N = 30)
  expect_no_error(facet_coverage_plot(sim_data_small$results_df, 
                                      true_val = sim_data_small$true_val))
})
