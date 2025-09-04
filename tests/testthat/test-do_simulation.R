test_that("do_simulation returns correct structure", {
  result <- do_simulation(N_sim = 5, data_N = 50)
  
  expect_true(is.list(result))
  expect_named(result, c("results_df", "true_val"))
  expect_s3_class(result$results_df, "data.frame")
  expect_true(is.numeric(result$true_val))

  expect_length(result$true_val, 1)
  expect_equal(nrow(result$results_df), 5)
  
  column_names <- c("maic_log_OR", "maic_se", "N", "ESS", "ESS_N_ratio", "Min",  
                  "Q1", "Median", "Q3", "Max", "Mean", "SD", "zero_weights", 
                  "greater_than_three_weights", "maic_ci_lower", "maic_ci_upper", 
                  "stc_log_OR", "stc_se", "stc_ci_lower", "stc_ci_upper", 
                  "bucher_log_OR", "bucher_se", "bucher_ci_lower", "bucher_ci_upper")

  expect_true(all(column_names %in% names(result$results_df)))
  expect_equal(ncol(result$results_df), length(column_names))
  expect_equal(nrow(result$results_df), 5)
})

