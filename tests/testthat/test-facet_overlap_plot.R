test_that("facet_overlap_plot returns ggplot object", {
  # Generate test data
  sim_data <- do_simulation(N_sim = 10, seed = 123, data_N = 50)
  
  # Test basic functionality
  plot <- facet_overlap_plot(sim_data$results_df)
  
  expect_s3_class(plot, "ggplot")
})

test_that("facet_overlap_plot handles different data sizes", {
  # Test with different simulation sizes
  sim_data_small <- do_simulation(N_sim = 3, seed = 123, data_N = 30)
  expect_no_error(facet_overlap_plot(sim_data_small$results_df))
  
  sim_data_medium <- do_simulation(N_sim = 20, seed = 123, data_N = 100)
  expect_no_error(facet_overlap_plot(sim_data_medium$results_df))
})

test_that("facet_overlap_plot handles edge cases", {
  # Test with minimal data
  sim_data_minimal <- do_simulation(N_sim = 2, seed = 123, data_N = 20)
  expect_no_error(facet_overlap_plot(sim_data_minimal$results_df))
})
