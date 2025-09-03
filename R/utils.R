#' Calculate Confidence Interval from Estimate and Standard Error
#'
#' This function calculates the confidence interval for a given estimate and standard error.
#' The estimate can be either a mean or a proportion.
#'
#' @param x Numeric. The estimate.
#' @param se Numeric. The standard error of the estimate.
#' @param alpha Numeric. The significance level for the CI (default is 0.05 for a 95% CI).
#'
#' @return A data.frame with 2 columns, lower and upper, representing the lower and upper 
#' bounds of the CI respectively
#'
#' @examples
#' ci_from_x_and_se(3, 0.2)
#'
#' @noRd
ci_from_x_and_se <- function(x, se, alpha = 0.05) {
  stopifnot(length(x) == length(se))
  
  z_value <- stats::qnorm(1 - alpha / 2) # For 95% CI
  lower <- x - z_value * se
  upper <- x + z_value * se
  
  data.frame(ci_lower = lower, ci_upper = upper)
}
