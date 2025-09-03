#' Function to calculate true values
#'
#' @param em_strength. A numeric value representing the effect modifier strength. Default
#' is 0.1.
#' @param overlap_param. A numeric value representing the overlap between the AB and AC trial
#' populations. Default is 0.5.
#' @param shared_em. A logical value indicating whether the shared effect modifier assumption
#' is true or not. Default is TRUE.
#'
#' @return A numeric value representing the B vs C true relative treatment effect
#'
#' @examples
#'
#' calc_true()
#'
#' @export
calc_true <- function(em_strength = 0.1, overlap_param = 0.5, shared_em = TRUE) {
  # Define parameters
  mu_X1 <- 1
  sigma_X1 <- 0.5
  mu_X2 <- 0.5
  sigma_X2 <- 0.1

  em_strength_other <- ifelse(em_strength == 0.1, 0.5, 0.1)

  b_trt <- -2
  b_X1 <- em_strength * sigma_X1
  b_X2 <- em_strength * sigma_X2

  c_trt <- -1.5
  if (shared_em) {
    c_X1 <- b_X1
    c_X2 <- b_X2
  } else {
    c_X1 <- em_strength_other * sigma_X1
    c_X2 <- em_strength_other * sigma_X2
  }

  # Expected mean of X1, X2 in AC population
  AC_X1_mean <- (1.1 + (1 - overlap_param)^2) * mu_X1
  AC_X2_mean <- (1.1 + (1 - overlap_param)^2) * mu_X2

  # Effect of B vs A in AC population
  d_AB_AC <- b_trt + b_X1 * (AC_X1_mean - mu_X1) + b_X2 * (AC_X2_mean - mu_X2)

  # Effect of C vs A in AC population
  d_AC_AC <- c_trt + c_X1 * (AC_X1_mean - mu_X1) + c_X2 * (AC_X2_mean - mu_X2)

  # Effect of C vs B in AC population
  d_BC_AC <- d_AC_AC - d_AB_AC

  return(d_BC_AC)
}
