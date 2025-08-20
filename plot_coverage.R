#' Function to plot maic coverage
#'
#' @param simulations_df A data.frame of simulate results. An output of `do_simulation()`
#' @param level A numerical decimal value representing the confidence level. Default is
#' 0.95
#' @param x_axis_lim A positive numeric representing the x axis scale limit. Default
#' is NULL, where limit is calculated automatically from the data.
#' @param plot_title A string representing the plot title. Default is NULL, where the
#' title is "MAIC Coverage".
#' @param show_legend A logical representing whether to include the legend in the plot.
#' Default is FALSE.
#' @param x_axis_min A numeric representing x axis minimum for the x axis scale. Default is NULL.
#' @param x_axis_max A numeric representing x axis maximum for the x axis scale. Default is NULL.
#' @param y_axis_min A numeric representing y axis minimum for the y axis scale. Default is NULL.
#' @param y_axis_max A numeric representing y axis maximum for the y axis scale. Default is NULL.
#'
#' @return A ggplot zip plot of confidence intervals.
#'
#' @examples
#' simulation_df <- do_simulation(10)
#' true_value <- 0.5
#' plot_maic_coverage(simulation_df$results_df, true_value, show_legend = TRUE)
#' @export

plot_maic_coverage <- function(simulation_df, true_value, level = 0.95,
                               x_axis_lim = NULL, plot_title = NULL, show_legend = FALSE) {

  plot_df <- simulation_df %>%
    dplyr::select(maic_log_OR, maic_ci_lower, maic_ci_upper, maic_se) %>%
    dplyr::mutate(
      ptruth = stats::pchisq(((maic_log_OR - true_value) / maic_se)^2, df = 1, lower.tail = FALSE),
      # Convert to percentile rank for y-axis positioning
      ypos = dplyr::percent_rank(dplyr::desc(ptruth)) * 100,
      # Flag whether interval covers truth
      is_covered = true_value <= maic_ci_upper & true_value >= maic_ci_lower
    )

  if (is.null(x_axis_lim)) {
    max_val <- max(abs(plot_df$maic_ci_lower - true_value), abs(plot_df$maic_ci_upper - true_value)) + 0.05
  } else {
    max_val <- x_axis_lim
  }

  ggplot2::ggplot(plot_df,
                ggplot2::aes(x = maic_ci_lower - true_value, xend = maic_ci_upper - true_value,
                             y = ypos, yend = ypos, colour = is_covered)) +
    # Reference lines
    ggplot2::geom_hline(yintercept = level*100, colour = "mediumpurple", size = 0.8) +  # Increased thickness
    ggplot2::geom_vline(xintercept = 0, colour = "grey60", size = 0.6) +  # Increased thickness
    # Plot intervals
    ggplot2::geom_segment(alpha = 0.4, size = 0.6) +  # Increased thickness of the intervals
    # Styling
    ggplot2::scale_y_continuous("Centile",
                              breaks = c((1 - level)*100, 50, level*100),
                              minor_breaks = NULL,
                              limits = c(0, 100)) +
    ggplot2::scale_x_continuous(limits = c(-max_val, max_val)) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = "lightblue", `FALSE` = "lightpink"),
      labels = c(`TRUE` = "Coverers", `FALSE` = "Non-Coverers"),
      name = " "
    ) +
    ggplot2::xlab("95% Confidence Intervals") +
    ggplot2::ggtitle(if(is.null(plot_title)) "MAIC coverage" else plot_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 11),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9),
      legend.position = if (show_legend) "bottom" else "none")
}

#' Function to plot stc coverage
#'
#' @param simulations_df A data.frame of simulate results. An output of `do_simulation()`
#' @param level A numerical decimal value representing the confidence level. Default is
#' 0.95
#' @param x_axis_lim A positive numeric representing the x axis scale limit. Default
#' is NULL, where limit is calculated automatically from the data.
#' @param plot_title A string representing the plot title. Default is NULL, where the
#' title is "STC Coverage".
#' @param show_legend A logical representing whether to include the legend in the plot.
#' Default is FALSE.
#'
#' @return A ggplot zip plot of confidence intervals.
#'
#' @examples
#' simulation_df <- do_simulation(10)
#' true_value <- 0.5
#' plot_stc_coverage(simulation_df$results_df, true_value)
#' @export

plot_stc_coverage <- function(simulation_df, true_value, level = 0.95,
                              x_axis_lim = NULL, plot_title = NULL, show_legend = FALSE) {

  plot_df <- simulation_df %>%
    dplyr::select(stc_log_OR, stc_ci_lower, stc_ci_upper, stc_se) %>%
    dplyr::mutate(
      ptruth = stats::pchisq(((stc_log_OR - true_value) / stc_se)^2, df = 1, lower.tail = FALSE),
      # Convert to percentile rank for y-axis positioning
      ypos = dplyr::percent_rank(dplyr::desc(ptruth)) * 100,
      # Flag whether interval covers truth
      is_covered = true_value <= stc_ci_upper & true_value >= stc_ci_lower
    )

  if (is.null(x_axis_lim)) {
    max_val <- max(abs(plot_df$stc_ci_lower - true_value), abs(plot_df$stc_ci_upper - true_value)) + 0.05
  } else {
    max_val <- x_axis_lim
  }

  ggplot2::ggplot(plot_df,
                  ggplot2::aes(x = stc_ci_lower - true_value, xend = stc_ci_upper - true_value,
                               y = ypos, yend = ypos, colour = is_covered)) +
    # Reference lines
    ggplot2::geom_hline(yintercept = level*100, colour = "mediumpurple", size = 0.8) +  # Increased thickness
    ggplot2::geom_vline(xintercept = 0, colour = "grey60", size = 0.6) +  # Increased thickness
    # Plot intervals
    ggplot2::geom_segment(alpha = 0.4, size = 0.6) +  # Increased thickness of the intervals
    # Styling
    ggplot2::scale_y_continuous("Centile",
                                breaks = c((1 - level)*100, 50, level*100),
                                minor_breaks = NULL,
                                limits = c(0, 100)) +
    ggplot2::scale_x_continuous(limits = c(-max_val, max_val)) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = "lightblue", `FALSE` = "lightpink"),
      labels = c(`TRUE` = "Coverers", `FALSE` = "Non-Coverers"),
      name = " "
    ) +
    ggplot2::xlab("95% Confidence Intervals") +
    ggplot2::ggtitle(if(is.null(plot_title)) "STC Coverage" else plot_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 11),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9),
      legend.position = if (show_legend) "bottom" else "none")
}

#' Function to plot bucher coverage
#'
#' @param simulations_df A data.frame of simulate results. An output of `do_simulation()`
#' @param level A numerical decimal value representing the confidence level. Default is
#' 0.95
#' @param x_axis_lim A positive numeric representing the x axis scale limit. Default
#' is NULL, where limit is calculated automatically from the data.
#' @param plot_title A string representing the plot title. Default is NULL, where the
#' title is "Bucher Coverage".
#' @param show_legend A logical representing whether to include the legend in the plot.
#' Default is FALSE.
#'
#' @return A ggplot zip plot of confidence intervals.
#'
#' @examples
#' simulation_df <- do_simulation(10)
#' true_value <- 0.5
#' plot_bucher_coverage(simulation_df$results_df, true_value = 0.5)
#'
#' @export

plot_bucher_coverage <- function(simulation_df, true_value, level = 0.95,
                                 x_axis_lim = NULL, plot_title = NULL, show_legend = FALSE) {

  plot_df <- simulation_df %>%
    dplyr::select(bucher_log_OR, bucher_ci_lower, bucher_ci_upper, bucher_se) %>%
    dplyr::mutate(
      ptruth = stats::pchisq(((bucher_log_OR - true_value) / bucher_se)^2, df = 1, lower.tail = FALSE),
      # Convert to percentile rank for y-axis positioning
      ypos = dplyr::percent_rank(dplyr::desc(ptruth)) * 100,
      # Flag whether interval covers truth
      is_covered = true_value <= bucher_ci_upper & true_value >= bucher_ci_lower
    )

  if (is.null(x_axis_lim)) {
    max_val <- max(abs(plot_df$bucher_ci_lower - true_value), abs(plot_df$bucher_ci_upper - true_value)) + 0.05
  } else {
    max_val <- x_axis_lim
  }

  ggplot2::ggplot(plot_df,
                  ggplot2::aes(x = bucher_ci_lower - true_value, xend = bucher_ci_upper - true_value,
                               y = ypos, yend = ypos, colour = is_covered)) +
    # Reference lines
    ggplot2::geom_hline(yintercept = level*100, colour = "mediumpurple", size = 0.8) +  # Increased thickness
    ggplot2::geom_vline(xintercept = 0, colour = "grey60", size = 0.6) +  # Increased thickness
    # Plot intervals
    ggplot2::geom_segment(alpha = 0.4, size = 0.6) +  # Increased thickness of the intervals
    # Styling
    ggplot2::scale_y_continuous("Centile",
                                breaks = c((1 - level)*100, 50, level*100),
                                minor_breaks = NULL,
                                limits = c(0, 100)) +
    ggplot2::scale_x_continuous(limits = c(-max_val, max_val)) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = "lightblue", `FALSE` = "lightpink"),
      labels = c(`TRUE` = "Coverers", `FALSE` = "Non-Coverers"),
      name = " "
    ) +
    ggplot2::xlab("95% Confidence Intervals") +
    ggplot2::ggtitle(if(is.null(plot_title)) "Bucher Coverage" else plot_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 11),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9),
      legend.position = if (show_legend) "bottom" else "none")
}

