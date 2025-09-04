  data <- gen_data(N = 100, seed = 123)
  result <- bucher(data$AB_IPD, data$AC_aggregate)

test_that("bucher returns correct structure", {
  data <- gen_data(N = 100, seed = 123)
  result <- bucher(data$AB_IPD, data$AC_aggregate)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 4)
  expect_true(all(c("bucher_log_OR", "bucher_se", "bucher_ci_lower", 
                    "bucher_ci_upper") %in% names(result)))
  
  expect_true(is.numeric(result$bucher_log_OR))
  expect_true(is.numeric(result$bucher_se))
  expect_true(is.numeric(result$bucher_ci_lower))
  expect_true(is.numeric(result$bucher_ci_upper))
})

test_that("bucher calculations are correct", {
  # Manual calculation of AC log OR
  AC_data <- data$AC_aggregate
  expected_d_AC_log <- log(AC_data$y_C_sum * (AC_data$N_A - AC_data$y_A_sum) / 
                          (AC_data$y_A_sum * (AC_data$N_C - AC_data$y_C_sum)))
  
  expected_var_d_AC_log <- 1/AC_data$y_A_sum + 1/(AC_data$N_A - AC_data$y_A_sum) + 
                           1/AC_data$y_C_sum + 1/(AC_data$N_C - AC_data$y_C_sum)
  
  # Manual calculation of AB log OR
  AB_IPD <- data$AB_IPD
  AB_y_sum <- AB_IPD %>% 
    dplyr::group_by(treatment) %>%
    dplyr::summarise(y_sum = sum(y)) %>%
    tidyr::pivot_wider(names_from = treatment, values_from = y_sum) %>%
    as.data.frame()
  
  N <- nrow(AB_IPD)
  expected_d_AB_log <- log(AB_y_sum$B * (N/2 - AB_y_sum$A) / 
                          (AB_y_sum$A * (N/2 - AB_y_sum$B)))
  
  expected_var_d_AB_log <- 1/AB_y_sum$B + 1/(N/2 - AB_y_sum$A) + 
                           1/AB_y_sum$A + 1/(N/2 - AB_y_sum$B)
  
  # Manual calculation of indirect comparison (C vs B)
  expected_bucher_log_OR <- expected_d_AC_log - expected_d_AB_log
  expected_bucher_var <- expected_var_d_AC_log + expected_var_d_AB_log
  expected_bucher_se <- sqrt(expected_bucher_var)
  
  # Test that calculations match
  expect_equal(result$bucher_log_OR, expected_bucher_log_OR, tolerance = 1e-12)
  expect_equal(result$bucher_se, expected_bucher_se, tolerance = 1e-12)
  
  # Test confidence interval calculation
  z_value <- qnorm(0.975) 
  expected_ci_lower <- expected_bucher_log_OR - z_value * expected_bucher_se
  expected_ci_upper <- expected_bucher_log_OR + z_value * expected_bucher_se
  
  expect_equal(result$bucher_ci_lower, expected_ci_lower, tolerance = 1e-12)
  expect_equal(result$bucher_ci_upper, expected_ci_upper, tolerance = 1e-12)
})


