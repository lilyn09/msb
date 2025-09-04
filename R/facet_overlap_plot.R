#' Function to plot overlap
#'
#' This function creates a faceted plot showing population overlap between AB and AC trials.
#'
#' @param mu_X1 Numeric. The mean of effect modifier X1
#' @param sigma_X1 Numeric. The standard deviation of effect modifier X1
#' @param mu_X2 Numeric. The mean of effect modifer X2
#' @param sigma_X2 Numeric. The standard deviation of effect modifer X2
#' @param N Numeric. The number of subjects in the population
#' @param seed Numeric. The random seed.
#' @param x_axis_max Numeric. The x-axis maximum value. Default is NULL, where it
#' is calculated automatically.
#' @param y_axis_max Numeric. The y-axis maximum value. Default is NULL, where it
#' is calculated automatically.
#' @param x_axis_min Numeric. The x-axis minimum value. Default is NULL, where it
#' is calculated automatically.
#' @param y_axis_min Numeric. The y-axis minimum value. Default is NULL, where it
#' is calculated automatically.
#' @param plot_title String representing the plot title. Default is "Varying
#' Population Overlap".
#' @param overlap_params A vector of numerics representing the overlap parameters.
#' Default is c(1, 0.5, 0.25).
#'
#' @return A ggplot of population overlap
#'
#' @examples
#' facet_overlap_plot()
#'
#' @export
facet_overlap_plot <- function(mu_X1 = 1, sigma_X1 = 0.5, mu_X2 = 0.5, sigma_X2 = 0.1,
                               N = 500, seed = 123,
                               x_axis_max = NULL, y_axis_max = NULL,
                               x_axis_min = NULL, y_axis_min = NULL,
                               plot_title = "Varying Population Overlap",
                               overlap_params = c(1, 0.5, 0.25)) {

  df <- data.frame()

  for (i in 1:length(overlap_params)) {

  set.seed(seed)

  # Generate AB study
  df_1 <- data.frame(
    X1 = rnorm(N, mu_X1, sigma_X1),
    X2 = rnorm(N, mu_X2, sigma_X2)) %>%
    dplyr::mutate(
      Population = "AB",
      Overlap = paste("Overlap =", overlap_params[i])
    )

  df_2 <- data.frame(
    X1 = rnorm(N, (1.1 + (1 - overlap_params[i])^2) * mu_X1, 0.75 * sigma_X1),
    X2 = rnorm(N, (1.1 + (1 - overlap_params[i])^2) * mu_X2, 0.75 * sigma_X2)) %>%
    dplyr::mutate(
      Population = "AC",
      Overlap = paste("Overlap =", overlap_params[i])
    )

  df <- rbind(df, df_1, df_2)
}

  df$Overlap <- factor(df$Overlap, levels = paste("Overlap =", overlap_params))

  if (is.null(x_axis_min)) {
    x_axis_min <- min(df$X1)
  }

  if (is.null(y_axis_min)) {
    y_axis_min <- min(df$X2)
  }

  if (is.null(x_axis_max)) {
    x_axis_max <- max(df$X1) * 1.05
  }

  if (is.null(y_axis_max)) {
    y_axis_max <- max(df$X2) * 1.05
  }

  ggplot2::ggplot(df, ggplot2::aes(x = X1, y = X2, color = Population, shape = Population)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::stat_ellipse(level = 0.95) +  # 95% confidence ellipses
    ggplot2::scale_color_manual(values = c("AB" = "mediumpurple", "AC" = "lightpink")) +
    ggplot2::scale_shape_manual(values = c("AB" = 16, "AC" = 17)) +
    ggplot2::facet_grid(. ~ Overlap) +
    ggplot2::labs(
      x = "X",
      y = "X'"
    ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "serif"),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "grey85", color = "grey30"),
    strip.text = ggplot2::element_text(color = "grey20", face = "bold"),
    strip.placement = "outside",
    panel.spacing = grid::unit(1, "lines"),
    plot.title = ggplot2::element_text(hjust = 0.5),
    panel.border = ggplot2::element_rect(color = "grey30", fill = NA, size = 0.5),
    legend.position = "bottom",
    axis.title = ggplot2::element_text(size = 11)
    ) +
  ggplot2::scale_x_continuous(limits = c(x_axis_min, x_axis_max)) +
  ggplot2::scale_y_continuous(limits = c(y_axis_min, y_axis_max)) +
  ggplot2::ggtitle(plot_title)
}

