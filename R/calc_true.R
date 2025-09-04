#' Function to calculate true values
#'
#' This function calculates the true treatment.
#'
#' @return A numeric value representing the B vs C true relative treatment effect
#'
#' @examples
#'
#' calc_true()
#'
#' @export
calc_true <- function() {
  # Define treatment effect parameters
  b_trt <- -2   # Treatment B vs A effect
  c_trt <- -1.5 # Treatment C vs A effect

  # Effect of C vs B = (C vs A) - (B vs A)
  # With shared effect modifiers, the covariate adjustment terms cancel out
  # d_BC_AC = (c_trt + effect_modifiers) - (b_trt + effect_modifiers) = c_trt - b_trt
  d_BC_AC <- c_trt - b_trt

  return(d_BC_AC)
}
