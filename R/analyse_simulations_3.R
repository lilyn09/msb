#' Function to analyse simulations
#'
#' @param sim_results A list of simulation results. The output from `do_simulation()`.
#'
#' @return A list containing data.frames `shared_results` and `maic_wts_results`
#'
#' @examples
#' sim_results <- do_simulation(10)
#'
#' analyse_simulations(sim_results)
analyse_simulations_3 <- function(sim_results) {
  results_df <- sim_results$results_df
  true_value <- sim_results$true_val
  shared_results <- results_df %>%
    dplyr::summarise(
      maic_abs_bias = mean(abs(maic_log_OR - true_value), na.rm = TRUE),
      maic_bias = mean((maic_log_OR - true_value), na.rm = TRUE),
      maic_bias_mcse = stats::sd(maic_log_OR, na.rm = TRUE) / sqrt(sum(!is.na(maic_log_OR))),
      maic_bias_pct = mean((maic_log_OR - true_value) / true_value * 100, na.rm = TRUE),
      maic_empSE = stats::sd(maic_log_OR, na.rm = TRUE),
      maic_empSE_mcse = if(sum(!is.na(maic_log_OR)) > 1) {
        stats::sd(maic_log_OR, na.rm = TRUE) / sqrt(2 * (sum(!is.na(maic_log_OR)) - 1))
      } else {
        NA
      },
      maic_modSE = sqrt(mean(maic_se^2, na.rm = TRUE)),
      maic_modSE_mcse = if(sum(!is.na(maic_se)) > 0 && mean(maic_se, na.rm = TRUE) != 0) {
        sqrt(stats::var(maic_se^2, na.rm = TRUE) / (4 * sum(!is.na(maic_se)) * mean(maic_se, na.rm = TRUE)^2))
      } else {
        NA
      },
      maic_MSE = mean((maic_log_OR - true_value)^2, na.rm = TRUE),
      maic_coverage = mean(maic_ci_lower <= true_value & maic_ci_upper >= true_value, na.rm = TRUE),
      maic_coverage_mcse = if(sum(!is.na(maic_ci_lower) & !is.na(maic_ci_upper)) > 0) {
        sqrt(mean(maic_ci_lower <= true_value & maic_ci_upper >= true_value, na.rm = TRUE) *
               (1 - mean(maic_ci_lower <= true_value & maic_ci_upper >= true_value, na.rm = TRUE)) /
               sum(!is.na(maic_ci_lower) & !is.na(maic_ci_upper)))
      } else {
        NA
      },
      stc_abs_bias = mean(abs(stc_log_OR - true_value), na.rm = TRUE),
      stc_bias = mean((stc_log_OR - true_value), na.rm = TRUE),
      stc_bias_mcse = stats::sd(stc_log_OR, na.rm = TRUE) / sqrt(sum(!is.na(stc_log_OR))),
      stc_bias_pct = mean(stc_log_OR - true_value / abs(true_value) * 100, na.rm = TRUE),
      stc_empSE = stats::sd(stc_log_OR, na.rm = TRUE),
      stc_empSE_mcse = if(sum(!is.na(stc_log_OR)) > 1) {
        stats::sd(stc_log_OR, na.rm = TRUE) / sqrt(2 * (sum(!is.na(stc_log_OR)) - 1))
      } else {
        NA
      },
      stc_modSE = sqrt(mean(stc_se^2, na.rm = TRUE)),
      stc_modSE_mcse = if(sum(!is.na(stc_se)) > 0 && mean(stc_se, na.rm = TRUE) != 0) {
        sqrt(stats::var(stc_se^2, na.rm = TRUE) / (4 * sum(!is.na(stc_se)) * mean(stc_se, na.rm = TRUE)^2))
      } else {
        NA
      },
      stc_MSE = mean((stc_log_OR - true_value)^2, na.rm = TRUE),
      stc_coverage = mean(stc_ci_lower <= true_value & stc_ci_upper >= true_value, na.rm = TRUE),
      stc_coverage_mcse = if(sum(!is.na(stc_ci_lower) & !is.na(stc_ci_upper)) > 0) {
        sqrt(mean(stc_ci_lower <= true_value & stc_ci_upper >= true_value, na.rm = TRUE) *
               (1 - mean(stc_ci_lower <= true_value & stc_ci_upper >= true_value, na.rm = TRUE)) /
               sum(!is.na(stc_ci_lower) & !is.na(stc_ci_upper)))
      } else {
        NA
      },
      bucher_abs_bias = mean(abs(bucher_log_OR - true_value), na.rm = TRUE),
      bucher_bias = mean((bucher_log_OR - true_value), na.rm = TRUE),
      bucher_bias_mcse = stats::sd(bucher_log_OR, na.rm = TRUE) / sqrt(sum(!is.na(bucher_log_OR))),
      bucher_bias_pct = mean((bucher_log_OR - true_value) / abs(true_value) * 100, na.rm = TRUE),
      bucher_empSE = stats::sd(bucher_log_OR, na.rm = TRUE),
      bucher_empSE_mcse = if(sum(!is.na(bucher_log_OR)) > 1) {
        stats::sd(bucher_log_OR, na.rm = TRUE) / sqrt(2 * (sum(!is.na(bucher_log_OR)) - 1))
      } else {
        NA
      },
      bucher_modSE = sqrt(mean(bucher_se^2, na.rm = TRUE)),
      bucher_modSE_mcse = if(sum(!is.na(bucher_se)) > 0 && mean(bucher_se, na.rm = TRUE) != 0) {
        sqrt(stats::var(bucher_se^2, na.rm = TRUE) / (4 * sum(!is.na(bucher_se)) * mean(bucher_se, na.rm = TRUE)^2))
      } else {
        NA
      },
      bucher_MSE = mean((bucher_log_OR - true_value)^2, na.rm = TRUE),
      bucher_coverage = mean(bucher_ci_lower <= true_value & bucher_ci_upper >= true_value, na.rm = TRUE),
      bucher_coverage_mcse = if(sum(!is.na(bucher_ci_lower) & !is.na(bucher_ci_upper)) > 0) {
        sqrt(mean(bucher_ci_lower <= true_value & bucher_ci_upper >= true_value, na.rm = TRUE) *
               (1 - mean(bucher_ci_lower <= true_value & bucher_ci_upper >= true_value, na.rm = TRUE)) /
               sum(!is.na(bucher_ci_lower) & !is.na(bucher_ci_upper)))
      } else {
        NA
      }
    )

  maic_wts_results <- results_df %>%
    dplyr::select("N", "ESS", "ESS_N_ratio",
                  "Min", "Q1", "Median", "Q3", "Max", "Mean", "SD",
                  "zero_weights", "greater_than_three_weights") %>%
    colMeans(na.rm = TRUE)

  list(shared_results = shared_results,
       maic_wts_results = maic_wts_results)
}
