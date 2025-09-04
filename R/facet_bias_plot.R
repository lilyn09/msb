#' Function to create a faceted bias plot
#'
#' @param datasets A list of data.frames of statistics from the simulation study.
#' @param facet_names A vector of facet names.
#' @param max_val A numeric representing the x-axis limit. Default is NULL, where
#' the value is calculated automatically from the data.
#' @param title A string representing the plot title. Default is NULL, where the
#' title is automatically "Bias Plot".
#'
#' @return A ggplot of MAIC, STC and Bucher bias values.
#'
#' @examples
#' simulation_df_1 <- do_simulation(10, data_overlap_param = 1)
#' simulation_df_2 <- do_simulation(10, data_overlap_param = 0.75)
#'
#' stats_df_1 <- analyse_simulations(simulation_df_1)$shared_results
#' stats_df_2 <- analyse_simulations(simulation_df_2)$shared_results
#'
#' datasets <- list(stats_df_1, stats_df_2)
#'
#' facet_bias_plot(datasets = datasets,
#'                 facet_names = c("1", "2"))
#'
#' @export
facet_bias_plot <- function(datasets, facet_names, max_val = NULL, title = NULL) {
  if(length(datasets) != length(facet_names)) {
    stop("The number of datasets must equal the number of facet names")
  }
  processed_dfs <- list()
  for(i in 1:length(datasets)) {
    processed_dfs[[i]] <- datasets[[i]] %>%
      dplyr::select(bucher_bias, maic_bias, stc_bias) %>%
      dplyr::rename(
        Bucher = bucher_bias,
        MAIC = maic_bias,
        STC = stc_bias
      ) %>%
      tidyr::pivot_longer(
        cols = c("Bucher", "MAIC", "STC"),
        names_to = "method"
      ) %>%
      dplyr::rename(
        bias = value
      ) %>%
      dplyr::mutate(
        facet = facet_names[i]
      )
  }
  dataset <- do.call(rbind, processed_dfs)

  dataset$facet <- factor(dataset$facet, levels = facet_names)
  dataset$method <- factor(dataset$method, levels = c("STC", "MAIC", "Bucher"))

  if (is.null(max_val)) {
    max_val <- max(dataset$bias, na.rm = TRUE)
  }

  exceeding_positive <- dataset %>%
    dplyr::filter(!is.na(bias) & bias > max_val) %>%
    dplyr::mutate(
      original_bias = bias,
      bias = max_val * 0.95
    )

  exceeding_negative <- dataset %>%
    dplyr::filter(!is.na(bias) & bias < -max_val) %>%
    dplyr::mutate(
      original_bias = bias,
      bias = -max_val * 0.95
    )

  na_dataset <- dataset %>%
    dplyr::filter(is.na(bias)) %>%
    dplyr::mutate(
      bias = max_val * 0.2
    )

  dataset_filtered <- dataset %>%
    dplyr::filter(!is.na(bias)) %>%
    dplyr::filter(bias <= max_val & bias >= -max_val)

  dataset_filtered$facet <- factor(dataset_filtered$facet, levels = facet_names)
  dataset_filtered$method <- factor(dataset_filtered$method, levels = c("STC", "MAIC", "Bucher"))

  if(nrow(na_dataset) > 0) {
    na_dataset$facet <- factor(na_dataset$facet, levels = facet_names)
    na_dataset$method <- factor(na_dataset$method, levels = c("STC", "MAIC", "Bucher"))
  }

  if(nrow(exceeding_positive) > 0) {
    exceeding_positive$facet <- factor(exceeding_positive$facet, levels = facet_names)
    exceeding_positive$method <- factor(exceeding_positive$method, levels = c("STC", "MAIC", "Bucher"))
  }

  if(nrow(exceeding_negative) > 0) {
    exceeding_negative$facet <- factor(exceeding_negative$facet, levels = facet_names)
    exceeding_negative$method <- factor(exceeding_negative$method, levels = c("STC", "MAIC", "Bucher"))
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 0, color = "grey30", size = 0.6) +
    ggplot2::geom_segment(
      data = dataset_filtered,
      ggplot2::aes(x = 0, xend = bias, y = method, yend = method),
      color = "grey50"
    ) +
    ggplot2::geom_point(
      data = dataset_filtered,
      ggplot2::aes(x = bias, y = method, fill = method),
      size = 4,
      stroke = 0.5,
      shape = 21,
      color = "grey30"
    )

  if(nrow(na_dataset) > 0) {
    p <- p + ggplot2::geom_text(
      data = na_dataset,
      ggplot2::aes(x = bias, y = method),
      label = "NA",
      color = "grey50",
      size = 3.5,
      fontface = "bold"
    )
  }

  p <- p + ggplot2::scale_fill_manual(values = c("Bucher" = "lightblue", "MAIC" = "lightpink", "STC" = "mediumpurple")) +
    ggplot2::facet_grid(facet ~ ., drop = FALSE) +  # Added drop = FALSE to preserve all levels
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none",
      strip.background = ggplot2::element_rect(fill = "grey85", color = "grey30"),
      strip.text = ggplot2::element_text(color = "grey20", face = "bold"),
      strip.placement = "outside",
      axis.title.y = ggplot2::element_blank(),
      panel.spacing = grid::unit(1, "lines"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.border = ggplot2::element_rect(color = "grey30", fill = NA, size = 0.5)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-max_val, max_val),
      name = "Bias"
    ) +
    ggplot2::ggtitle(ifelse(is.null(title), "Bias Plot", title))

  if(nrow(exceeding_positive) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = exceeding_positive,
        ggplot2::aes(x = 0, xend = max_val, y = method, yend = method),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "closed"),
        color = "grey50",
        linetype = "dashed"
      ) +
      ggplot2::geom_text(
        data = exceeding_positive,
        ggplot2::aes(x = max_val - max_val * 0.1, y = method,
                     label = paste0("[", round(original_bias, 3), "]")),
        color = "grey30",
        size = 3,
        vjust = 1.5
      )
  }

  if(nrow(exceeding_negative) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = exceeding_negative,
        ggplot2::aes(x = 0, xend = -max_val, y = method, yend = method),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "closed"),
        color = "grey50",
        linetype = "dashed"
      ) +
      ggplot2::geom_text(
        data = exceeding_negative,
        ggplot2::aes(x = -max_val + max_val * 0.1, y = method,
                     label = paste0("[", round(original_bias, 3), "]")),
        color = "grey30",
        size = 3,
        vjust = 1.5
      )
  }

  return(p)
}
