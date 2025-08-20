#' Function to plot overlap
#'
#' @param mu_X1 Numeric. The mean of effect modifier X1
#' @param sigma_X1 Numeric. The standard deviation of effect modifier X1
#' @param mu_X2 Numeric. The mean of effect modifer X2
#' @param sigma_X2 Numeric. The standard deviation of effect modifer X2
#' @param N Numeric. The number of subjects in the population
#' @param overlap_param Numeric. Representing the amount of population overlap.
#' @param seed Numeric. The random seed.
#'
#' @return A ggplot of population overlap
#'
#' @examples
#' overlap_plot()
#'
#' @export
overlap_plot <- function(mu_X1 = 1, sigma_X1 = 0.5, mu_X2 = 0.5, sigma_X2 = 0.1,
                         N = 500, overlap_param = 0.75, seed = 123,
                         x_axis_max = NULL, y_axis_max = NULL,
                         x_axis_min = NULL, y_axis_min = NULL) {

set.seed(seed)

# Generate AB study
X1 <- rnorm(N, mu_X1, sigma_X1)
X2 <- rnorm(N, mu_X2, sigma_X2)

# Generate AC study
X1_overlap <- rnorm(N, (1.1 + (1 - overlap_param)^2) * mu_X1, 0.75 * sigma_X1)
X2_overlap <- rnorm(N, (1.1 + (1 - overlap_param)^2) * mu_X2, 0.75 * sigma_X2)

df <- data.frame(
  X1 = c(X1, X1_overlap),
  X2 = c(X2, X2_overlap),
  Population = factor(c(rep("AB", N), rep("AC", N)))
)

if (is.null(x_axis_min)) {
  x_axis_min <- 0  # Start from 0 by default
}

if (is.null(y_axis_min)) {
  y_axis_min <- 0  # Start from 0 by default
}

if (is.null(x_axis_max)) {
  x_axis_max <- max(df$X1) * 1.05  # Add 5% padding
}

if (is.null(y_axis_max)) {
  y_axis_max <- max(df$X2) * 1.05  # Add 5% padding
}

ggplot2::ggplot(df, ggplot2::aes(x = X1, y = X2, color = Population, shape = Population)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::scale_color_manual(values = c("AB" = "mediumpurple", "AC" = "lightpink")) +
  ggplot2::scale_shape_manual(values = c("AB" = 16, "AC" = 17)) +
  ggplot2::labs(
    title = paste0("Overlap = ", overlap_param),
    x = "X",
    y = "X'"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "serif"),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(hjust = 0.5, size = 11),
    axis.title = ggplot2::element_text(size = 8)
  ) +
  ggplot2::scale_x_continuous(limits = c(x_axis_min, x_axis_max)) +
  ggplot2::scale_y_continuous(limits = c(y_axis_min, y_axis_max))

}
