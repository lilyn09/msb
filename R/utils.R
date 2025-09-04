#' Calculate Confidence Interval
#'
#' This function calculates the confidence interval for a given estimate and standard error.
#'
#' @param x A numeric value representing the estimate.
#' @param se A numeric value representing the standard error of the estimate.
#' @param alpha A numeric value representing the significance level for the confidence interval (default is 0.05).
#'
#' @return A data.frame with 2 columns, lower and upper, representing the lower and upper 
#' bounds of the confidence interval respectively
#'
#' @examples
#' msb:::calc_ci(3, 0.2)
#'
#' @noRd
calc_ci <- function(x, se, alpha = 0.05) {
  stopifnot(length(x) == length(se))
  
  z_value <- stats::qnorm(1 - alpha / 2)
  lower <- x - z_value * se
  upper <- x + z_value * se
  
  data.frame(ci_lower = lower, ci_upper = upper)
}
