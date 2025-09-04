#' Function to run simulations
#'
#' @param N_sim. An integer value representing the number of times to repeat each simulation.
#' Default is 1000.
#' @param seed. An integer value representing the random seed. Default is 1234.
#' @param data_N An integer value representing the number of subjects in each trial.
#' Default is 500.
#' @param data_em_strength A numeric value representing the effect modifier strength.
#' Default is 0.1.
#' @param data_overlap_param A numeric value representing the overlap between the AB
#' and AC trial populations. Default is 0.5.
#' @param data_adjust_all A logical value indicating whether to adjust by all effect
#' modifiers. Default is TRUE.
#' @param wts_type A character string specifying the type of weights to use in MAIC. 
#' Options are "rescaled_weights" or "weights". "rescaled_weights" 
#' rescales the weights so they sum to the original sample size, while "weights" 
#' uses the raw propensity score weights. Default is "rescaled weights".
#'
#' @return A list containing a data.frame `results_df` and numeric `true_val`
#'
#' @examples
#' do_simulation(10)
#'
#' @export

do_simulation <- function(N_sim = 1000, seed = 1234, data_N = 500, data_em_strength = 0.5,
                          data_overlap_param = 0.5, data_adjust_all = TRUE,
                          wts_type = c("rescaled_weights", "weights")) {
  type <- match.arg(wts_type)

  set.seed(seed)
  sim_seeds <- sample.int(.Machine$integer.max, N_sim, replace = FALSE)

  true_val <- calc_true()

  results_df <- data.frame(matrix(nrow = N_sim, ncol = 24))
  colnames(results_df) <- c("maic_log_OR", "maic_se", "N", "ESS", "ESS_N_ratio",
                            "Min", "Q1", "Median", "Q3", "Max", "Mean", "SD",
                            "zero_weights", "greater_than_three_weights", "maic_ci_lower",
                            "maic_ci_upper", "stc_log_OR", "stc_se", "stc_ci_lower",
                            "stc_ci_upper", "bucher_log_OR", "bucher_se", "bucher_ci_lower",
                            "bucher_ci_upper")

  for(i in 1:N_sim) {
    sim_data <- gen_data(N = data_N,
                         em_strength = data_em_strength,
                         overlap_param = data_overlap_param,
                         seed = sim_seeds[i])
    results_df[i, 1:16] <- maic(sim_data$AB_IPD, sim_data$AC_aggregate, adjust_all = data_adjust_all, type = type)
    results_df[i, 17:20] <- stc(sim_data$AB_IPD, sim_data$AC_aggregate, adjust_all = data_adjust_all)
    results_df[i, 21:24] <- bucher(sim_data$AB_IPD, sim_data$AC_aggregate)
  }

  list(results_df = results_df,
       true_val = true_val)
}
