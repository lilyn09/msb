#' Function to plot bias
#'
#' @param stats_df A data.frame of statistics from the simulation study. An output
#' of `analyse_statistics()`.
#' @param x_axis_lim A positive numeric representing the x axis scale limit. Default
#' is NULL, where limit is calculated automatically from the data.
#' @param plot_title A string representing the plot title. Default is NULL, where the
#' title is "Biases Plot".
#'
#' @return A ggplot of MAIC, STC and Bucher bias values.
#'
#' @examples
#' simulation_df <- do_simulation(10)
#' stats_df <- analyse_simulations(simulation_df)$shared_results
#' plot_bias(stats_df)
#'
#' @export

plot_bias <- function(stats_df, x_axis_lim = NULL, plot_title = NULL) {
  plot_df <- stats_df %>%
    dplyr::select(maic_bias, stc_bias, bucher_bias) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "bias_type",
      values_to = "bias_value"
    )  %>%
    dplyr::mutate(bias_type = dplyr::case_when(
      bias_type == "maic_bias" ~ "MAIC",
      bias_type == "stc_bias" ~ "STC",
      bias_type == "bucher_bias" ~ "Bucher",
      TRUE ~ bias_type
    ),
      is_na = is.na(bias_value)
    )

  non_na_values <- plot_df$bias_value[!is.na(plot_df$bias_value)]

  if (is.null(x_axis_lim)) {
    max_val <- if(length(non_na_values) > 0) max(abs(non_na_values) + 0.05) else 1
  } else {
    max_val <- x_axis_lim
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(y = bias_type)) +
    ggplot2::geom_segment(
      data= plot_df %>% dplyr::filter(!is_na),
      ggplot2::aes(x = 0, xend = bias_value, y = bias_type, yend = bias_type),
      color = "grey", size = 1) +
    ggplot2::geom_point(
      data = plot_df %>% dplyr::filter(!is_na),
      ggplot2::aes(x = bias_value, color = bias_type),
      size = 4) +
    ggplot2::geom_text(
        data = plot_df %>% dplyr::filter(is_na),
        ggplot2::aes(x = 0.05, y = bias_type),
        label = "NA",
        color = "darkgrey",
        fontface = "bold",
        size = 3.5
      ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::scale_x_continuous(limits = c(-max_val, max_val)) +
    ggplot2::scale_color_manual(values = c("MAIC" = "lightpink",
                                  "STC" = "mediumpurple",
                                  "Bucher" = "lightblue")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 11, hjust = 0.5, face = "bold"),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9)) +
    ggplot2::labs(title = if(is.null(plot_title)) "Biases Plot" else plot_title,
                  x = "Bias",
                  y = NULL)
}



