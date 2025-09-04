#' Function to create a faceted coverage plot
#'
#' @param datasets A list of data.frames of statistics from the simulation study.
#' @param facet_names A vector of facet names.
#' @param max_val A numeric representing the x-axis limit. Default is NULL, where
#' the value is calculated automatically from the data.
#' @param title A string representing the plot title. Default is NULL, where the
#' title is automatically "Coverage Plot".
#' @param level A numeric representing the nominal confidence interval level. Default
#' is 0.95.
#' @param true_val A numeric representing the true B vs C treatment effect. Default is
#' 0.5
#'
#' @return A ggplot of MAIC, STC and Bucher coverage values.
#'
#' @examples
#' simulation_df_1 <- do_simulation(10, data_overlap_param = 1)$results_df
#' simulation_df_2 <- do_simulation(10, data_overlap_param = 0.75)$results_df
#' 
#' datasets <- list(simulation_df_1, simulation_df_2)
#'
#' facet_coverage_plot(datasets = datasets,
#'                 facet_names = c("1", "2"))
#'
#' @export
facet_coverage_plot <- function(datasets, facet_names, max_val = NULL, title = NULL, level = 0.95,
                                true_val = 0.5) {
  if(length(datasets) != length(facet_names)) {
    stop("The number of datasets must equal the number of facet names")
  }

  methods <- c("maic", "stc", "bucher")

  df_list <- list()

  for (i in 1:length(datasets)) {
    dataset <- datasets[[i]]
    current_facet <- facet_names[i]

    for (method in methods) {
      log_OR_col <- paste0(method, "_log_OR")
      ci_lower_col <- paste0(method, "_ci_lower")
      ci_upper_col <- paste0(method, "_ci_upper")
      se_col <- paste0(method, "_se")

      method_df <- dataset %>%
        dplyr::select(!!rlang::sym(log_OR_col), !!rlang::sym(ci_lower_col),
                      !!rlang::sym(ci_upper_col), !!rlang::sym(se_col)) %>%
        dplyr::mutate(
          ptruth = stats::pchisq(((!!rlang::sym(log_OR_col) - true_val) / !!rlang::sym(se_col))^2,
                                 df = 1, lower.tail = FALSE),
          ypos = dplyr::percent_rank(dplyr::desc(ptruth)) * 100,
          is_covered = true_val <= !!rlang::sym(ci_upper_col) & true_val >= !!rlang::sym(ci_lower_col)
        ) %>%
        dplyr::mutate(
          method = dplyr::case_when(
            method == "maic" ~ "MAIC",
            method == "stc" ~ "STC",
            method == "bucher" ~ "Bucher",
            TRUE ~ method
          ),
          facet = current_facet
        )

      names(method_df)[names(method_df) == ci_lower_col] <- "ci_lower"
      names(method_df)[names(method_df) == ci_upper_col] <- "ci_upper"

      method_df <- method_df %>%
        dplyr::select(ci_lower, ci_upper, ypos, is_covered, method, facet)

      df_list <- append(df_list, list(method_df))
    }
  }

  df <- do.call(rbind, df_list)

  if (is.null(max_val)) {
    max_val <- max(abs(df$ci_lower - true_val), abs(df$ci_upper - true_val), na.rm = TRUE) + 0.05
  }

  df$method <- factor(df$method, levels = c("Bucher", "MAIC", "STC"))
  df$facet <- factor(df$facet, levels = facet_names)

  df <- df[!is.na(df$is_covered), ]

  ggplot2::ggplot(df,
                  ggplot2::aes(x = ci_lower - true_val, xend = ci_upper - true_val,
                               y = ypos, yend = ypos, colour = is_covered)) +
    ggplot2::geom_segment() +
    ggplot2::geom_hline(yintercept = level*100, colour = "mediumpurple", size = 0.8) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60", size = 0.6) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = "lightblue", `FALSE` = "lightpink"),
      labels = c(`TRUE` = "Contains true value", `FALSE` = "Does not contain true value")) +
    ggplot2::facet_grid(facet ~ method) +
    ggplot2::labs(
      x = paste0(level * 100, "% Confidence Intervals"),
      y = "Centile",
      color = NULL
    ) +
    ggplot2::scale_x_continuous(limits = c(-max_val, max_val)) +
    ggplot2::ggtitle(if(is.null(title)) "Coverage Plot" else title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 11),
      strip.text.x = ggplot2::element_text(size = 11, color = "grey20", face = "bold"),
      strip.text.y = ggplot2::element_text(size = 11, color = "grey20", face = "bold"),
      strip.background.x = ggplot2::element_rect(fill = "gray80", color = "grey30"),
      strip.background.y = ggplot2::element_rect(fill = "gray80", color = "grey30"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = grid::unit(1, "lines"),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      panel.border = ggplot2::element_rect(color = "grey30", fill = NA, size = 0.5)
    )
}

