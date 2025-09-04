sim_data <- do_simulation(N_sim = 5, seed = 123, data_N = 50)
stats_data <- analyse_simulations(sim_data)
result <- format_table(stats_data$shared_results, dp =4)

test_that("format_table returns correct structure", {
  expect_s3_class(result, "flextable")
  
  df <- result$body$dataset
  expect_s3_class(df, "data.frame")
  expected_colnames <- c("Section", "Statistic", "MAIC", "STC", "Bucher")
  expect_true(all(colnames(df) %in% expected_colnames))

  expected_statistics <- c("Bias", "Bias MCSE", "Bias PCE", "Empirical SE", "Empirical SE MCSE", 
  "Model SE", "Model SE MCSE", "MSE", "Coverage", "Coverage MCSE")
  expect_true(all(df$Statistic %in% expected_statistics))
})

test_that("rounds to correct number of dp", {
 expect_equal(df$MAIC, round(df$MAIC, digits = 4))
 expect_equal(df$STC, round(df$STC, digits = 4))
 expect_equal(df$Bucher, round(df$Bucher, digits = 4))
})