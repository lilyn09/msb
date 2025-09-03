#' Function to generate table of study statistics
#'
#' @param stats_df A data.frame of statistics from the simulation study. An output
#' of `analyse_statistics()`.
#' @param dp An integer representing the number of decimal places to round statistics
#' in the table to. Default is 3.
#'
#' @return a flextable
#'
#' @examples
#' simulation_df <- do_simulation(10)
#' stats_df <- analyse_simulations(simulation_df)$shared_results
#' format_table(stats_df)
#'
#' @export
format_table <- function(stats_df, dp = 3) {
  table_df <- data.frame(
    Section = c("Bias", "", "",
                "Empirical SE", "",
                "Model SE", "",
                "MSE",
                "Coverage", ""),
    Statistic = c("Bias", "Bias MCSE", "Bias PCE", "Empirical SE", "Empirical SE MCSE",
                "Model SE","Model SE MCSE", "MSE", "Coverage", "Coverage MCSE"),
    MAIC = c(stats_df$maic_bias, stats_df$maic_bias_mcse, stats_df$maic_bias_pct,
           stats_df$maic_empSE, stats_df$maic_empSE_mcse, stats_df$maic_modSE,
           stats_df$maic_modSE_mcse, stats_df$maic_MSE, stats_df$maic_coverage,
           stats_df$maic_coverage_mcse),
    STC = c(stats_df$stc_bias, stats_df$stc_bias_mcse, stats_df$stc_bias_pct,
           stats_df$stc_empSE, stats_df$stc_empSE_mcse, stats_df$stc_modSE,
           stats_df$stc_modSE_mcse, stats_df$stc_MSE, stats_df$stc_coverage,
           stats_df$stc_coverage_mcse),
    Bucher = c(stats_df$bucher_bias, stats_df$bucher_bias_mcse, stats_df$bucher_bias_pct,
           stats_df$bucher_empSE, stats_df$bucher_empSE_mcse, stats_df$bucher_modSE,
           stats_df$bucher_modSE_mcse, stats_df$bucher_MSE, stats_df$bucher_coverage,
           stats_df$bucher_coverage_mcse),
    check.names = FALSE
  ) %>%
  dplyr::mutate_if(is.numeric, ~ round(., dp))

ft <- flextable::flextable(table_df) %>%
  flextable::theme_vanilla() %>%
  flextable::colformat_double(j = 3:5, digits = 3)

ft
}
