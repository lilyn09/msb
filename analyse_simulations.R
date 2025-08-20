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

analyse_simulations <- function(sim_results) {

  results_df <- sim_results$results_df

  true_value <- sim_results$true_val

  shared_results <- results_df %>%
    dplyr::summarise(
      maic_bias = mean(maic_log_OR - true_value),
      maic_bias_mcse = stats::sd(maic_log_OR) / sqrt(nrow(results_df)),
      maic_bias_pct = mean((maic_log_OR - true_value) / abs(true_value)) * 100,
      maic_empSE = stats::sd(maic_log_OR),
      maic_empSE_mcse = maic_empSE / sqrt(2 * (nrow(results_df) - 1)),
      maic_modSE = sqrt(mean(maic_se^2)),
      maic_modSE_mcse = sqrt(stats::var(maic_se^2) / (4 * nrow(results_df) * mean(maic_se)^2)),
      maic_MSE = mean((maic_log_OR - true_value)^2),
      maic_coverage = mean(maic_ci_lower <= true_value & maic_ci_upper >= true_value),
      maic_coverage_mcse = sqrt(maic_coverage * (1 - maic_coverage) / nrow(results_df)),
      stc_bias = mean(stc_log_OR - true_value),
      stc_bias_mcse = stats::sd(stc_log_OR) / sqrt(nrow(results_df)),
      stc_bias_pct = mean((stc_log_OR - true_value) / abs(true_value)) * 100,
      stc_empSE = stats::sd(stc_log_OR),
      stc_empSE_mcse = stc_empSE / sqrt(2 * (nrow(results_df) - 1)),
      stc_modSE = sqrt(mean(stc_se^2)),
      stc_modSE_mcse = sqrt(stats::var(stc_se^2) / (4 * nrow(results_df) * mean(stc_se)^2)),
      stc_MSE = mean((stc_log_OR - true_value)^2),
      stc_coverage = mean(stc_ci_lower <= true_value & stc_ci_upper >= true_value),
      stc_coverage_mcse = sqrt(stc_coverage * (1 - stc_coverage) / nrow(results_df)),
      bucher_bias = mean(bucher_log_OR - true_value),
      bucher_bias_mcse = stats::sd(bucher_log_OR) / sqrt(nrow(results_df)),
      bucher_bias_pct = mean((bucher_log_OR - true_value) / abs(true_value)) * 100,
      bucher_empSE = stats::sd(bucher_log_OR),
      bucher_empSE_mcse = bucher_empSE / sqrt(2 * (nrow(results_df) - 1)),
      bucher_modSE = sqrt(mean(bucher_se^2)),
      bucher_modSE_mcse = sqrt(stats::var(bucher_se^2) / (4 * nrow(results_df) * mean(bucher_se)^2)),
      bucher_MSE = mean((bucher_log_OR - true_value)^2),
      bucher_coverage = mean(bucher_ci_lower <= true_value & bucher_ci_upper >= true_value),
      bucher_coverage_mcse = sqrt(bucher_coverage * (1 - bucher_coverage) / nrow(results_df)),
  )

  shared_results <- shared_results %>%
   dplyr::mutate(across(dplyr::everything(), ~ dplyr::if_else(. > 100, NA_real_, .)))

  maic_wts_results <- results_df %>%
    dplyr::select("N", "ESS", "ESS_N_ratio",
                  "Min", "Q1", "Median", "Q3", "Max", "Mean", "SD",
                  "zero_weights", "greater_than_three_weights") %>%
    colMeans(, na.rm = TRUE)

  list(shared_results = shared_results,
       maic_wts_results = maic_wts_results)
}

