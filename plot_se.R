#' Function to plot se
#'
#' @param stats_df A data.frame of statistics from the simulation study. An output
#' of `analyse_statistics()`.
#' @param x_axis_lim A positive numeric representing the x axis scale limit. Default
#' is NULL, where limit is calculated automatically from the data.
#' @param plot_title A string representing the plot title. Default is NULL, where the
#' title is "Empirical vs Model Standard Errors".
#' @param show_legend A logical representing whether to include the legend in the plot.
#' Default is FALSE.
#'
#' @return A ggplot of MAIC, STC and Bucher empirical and model SE values.
#'
#' @examples
#' simulation_df <- do_simulation(10)
#' stats_df <- analyse_simulations(simulation_df)$shared_results
#' plot_se(stats_df)
#'
#' @export

plot_se <- function(stats_df, x_axis_lim = NULL, plot_title = NULL, show_legend = FALSE) {
  plot_df <- stats_df %>%
    dplyr::select(maic_empSE, maic_modSE, stc_empSE, stc_modSE, bucher_empSE, bucher_modSE) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "SE_Type", values_to = "SE_Value") %>%
    dplyr::mutate(
      Method = dplyr::case_when(
        grepl("maic", SE_Type) ~ "MAIC",
        grepl("stc", SE_Type) ~ "STC",
        grepl("bucher", SE_Type) ~ "Bucher"
      ),
      SE_Category = ifelse(grepl("empSE", SE_Type), "Empirical", "Model")
    )

  na_check <- stats_df %>%
    dplyr::summarise(
      MAIC_both_NA = is.na(maic_empSE) & is.na(maic_modSE),
      STC_both_NA = is.na(stc_empSE) & is.na(stc_modSE),
      Bucher_both_NA = is.na(bucher_empSE) & is.na(bucher_modSE)
    ) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Method_NA", values_to = "Both_NA") %>%
    dplyr::mutate(
      Method = gsub("_both_NA", "", Method_NA)
    )

  plot_df <- plot_df %>%
    dplyr::filter(!is.na(SE_Value))

  if (is.null(x_axis_lim)) {
    x_max_limit <- if(nrow(plot_df) > 0) max(plot_df$SE_Value, na.rm = TRUE) + 0.05 else 1
  } else {
    x_max_limit <- x_axis_lim
  }

  na_annotations <- na_check %>%
    dplyr::filter(Both_NA == TRUE) %>%
    dplyr::mutate(
      x = x_max_limit / 2,
      label = "NA"
    )

  fill_colors <- c("MAIC" = "lightpink", "STC" = "mediumpurple", "Bucher" = "lightblue")

  ggplot2::ggplot(plot_df, ggplot2::aes(x = SE_Value, y = Method, fill = Method, shape = SE_Category)) +
    ggplot2::geom_point(
      size = 4,
      stroke = 0.5,
      color = "black"
    ) +
    ggplot2::geom_text(
      data = na_annotations,
      ggplot2::aes(x = x, y = Method, label = label),
      size = 3.5,
      fontface = "bold",
      color = "darkgrey",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = fill_colors, guide = "none") +
    ggplot2::scale_shape_manual(
      values = c(22, 21),
      guide = if (show_legend) "legend" else "none") +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, x_max_limit)) +
    ggplot2::labs(
      title = if(is.null(plot_title)) "Empirical vs Model Standard Errors" else plot_title,
      x = "Standard Error",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      plot.title = ggplot2::element_text(size = 11, hjust = 0.5),
      axis.title.x = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_blank(),
      legend.position = if (show_legend) "bottom" else "none",
      legend.text = ggplot2::element_text(size = 9),
      panel.grid.major.x = ggplot2::element_line(),
      panel.grid.major.y = ggplot2::element_line()
    )
}
