#' Function to create a faceted se plot
#'
#' This function creates a faceted plot displaying standard error statistics for MAIC, STC, and Bucher methods across different simulation scenarios.
#'
#' @param datasets A list of data.frames of statistics from the simulation study.
#' @param facet_names A vector of facet names.
#' @param max_val A numeric representing the x-axis limit. Default is NULL, where
#' the value is calculated automatically from the data.
#' @param title A string representing the plot title. Default is NULL, where the
#' title is automatically "Standard Errors Plot".
#'
#' @return A ggplot of MAIC, STC and Bucher se values.
#'
#' @examples
#' simulation_df_1 <- do_simulation(10, data_overlap_param = 1)
#' simulation_df_2 <- do_simulation(10, , data_overlap_param = 0.75)
#'
#' stats_df_1 <- analyse_simulations(simulation_df_1)$shared_results
#' stats_df_2 <- analyse_simulations(simulation_df_2)$shared_results
#'
#' datasets <- list(stats_df_1, stats_df_2)
#'
#' facet_se_plot(datasets = datasets,
#'                 facet_names = c("1", "2"))
#'
#' @export
facet_se_plot <- function(datasets, facet_names, max_val = NULL, title = NULL) {
  if(length(datasets) != length(facet_names)) {
    stop("The number of datasets must equal the number of facet names")
  }
  processed_dfs <- list()
  for(i in 1:length(datasets)) {
    processed_dfs[[i]] <- datasets[[i]] %>%
      dplyr::select(maic_empSE, maic_modSE, stc_empSE, stc_modSE, bucher_empSE, bucher_modSE) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "SE_Type", values_to = "SE_Value") %>%
      dplyr::mutate(
        Method = dplyr::case_when(
          grepl("maic", SE_Type) ~ "MAIC",
          grepl("stc", SE_Type) ~ "STC",
          grepl("bucher", SE_Type) ~ "Bucher"
        ),
        SE_Category = ifelse(grepl("empSE", SE_Type), "Empirical", "Model")
      ) %>%
      dplyr::mutate(
        facet = facet_names[i]
      ) %>%
      dplyr::select(Method, facet, SE_Category, SE_Value)
  }
  dataset <- do.call(rbind, processed_dfs)
  if (is.null(max_val)) {
    max_val <- max(dataset$SE_Value, na.rm = TRUE)
  }

  exceeding_vals <- dataset %>%
    dplyr::filter(!is.na(SE_Value) & SE_Value > max_val) %>%
    dplyr::mutate(
      original_SE_Value = SE_Value
    )

  na_dataset <- dataset %>%
    dplyr::filter(is.na(SE_Value)) %>%
    dplyr::group_by(Method, facet) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      SE_Value = max_val * 0.2
    )

  dataset_filtered <- dataset %>%
    dplyr::filter(!is.na(SE_Value)) %>%
    dplyr::filter(SE_Value <= max_val)

  dataset$Method <- factor(dataset$Method, levels = c("STC", "MAIC", "Bucher"))
  dataset$facet <- factor(dataset$facet, levels = facet_names)
  dataset$SE_Category <- factor(dataset$SE_Category, levels = c("Empirical", "Model"))
  dataset_filtered$Method <- factor(dataset_filtered$Method, levels = c("STC", "MAIC", "Bucher"))
  dataset_filtered$facet <- factor(dataset_filtered$facet, levels = facet_names)
  dataset_filtered$SE_Category <- factor(dataset_filtered$SE_Category, levels = c("Empirical", "Model"))
  na_dataset$facet <- factor(na_dataset$facet, levels = facet_names)

  if(nrow(exceeding_vals) > 0) {
    exceeding_vals$Method <- factor(exceeding_vals$Method, levels = c("STC", "MAIC", "Bucher"))
    exceeding_vals$facet <- factor(exceeding_vals$facet, levels = facet_names)
    exceeding_vals$SE_Category <- factor(exceeding_vals$SE_Category, levels = c("Empirical", "Model"))
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = dataset_filtered,
      ggplot2::aes(x = SE_Value, y = Method, fill = Method, shape = SE_Category),
      size = 4,
      stroke = 0.5,
      color = "grey20"
    ) +
    ggplot2::geom_text(
      data = na_dataset,
      ggplot2::aes(x = SE_Value, y = Method),
      label = "NA",
      color = "grey50",
      size = 3.5,
      fontface = "bold"
    ) +
    ggplot2::facet_grid(facet ~ .) +
    ggplot2::scale_shape_manual(values = c(22, 21)) +
    ggplot2::scale_fill_manual(values = c("Bucher" = "lightblue", "MAIC" = "lightpink", "STC" = "mediumpurple")) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey85", color = "grey30"),
      strip.text = ggplot2::element_text(color = "grey20", face = "bold"),
      strip.placement = "outside",
      panel.spacing = grid::unit(1, "lines"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.text.y.left = ggplot2::element_text(angle = 0),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "bottom",
      panel.border = ggplot2::element_rect(color = "grey30", fill = NA, size = 0.5)
    ) +
    ggplot2::labs(
      title = ifelse(is.null(title), "Standard Errors Plot", title),
      x = "Standard Errors",
      shape = ""
    ) +
    ggplot2::scale_x_continuous(limits = c(0, max_val))

  if(nrow(exceeding_vals) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = exceeding_vals,
        ggplot2::aes(x = 0, xend = max_val, y = Method, yend = Method, group = interaction(Method, SE_Category)),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "closed"),
        color = "grey50",
        linetype = "dashed"
      ) +
      ggplot2::geom_text(
        data = exceeding_vals,
        ggplot2::aes(x = max_val - max_val * 0.1, y = Method,
                     label = paste0("[",
                                    ifelse(original_SE_Value > 100,
                                           format(original_SE_Value, scientific = TRUE, digits = 4),
                                           round(original_SE_Value, 3)),
                                    "]"),
                     group = interaction(Method, SE_Category)),
        color = "grey30",
        size = 3,
        vjust = ifelse(exceeding_vals$SE_Category == "Empirical", -0.5, 1.5)
      )
  }

  return(p)
}
