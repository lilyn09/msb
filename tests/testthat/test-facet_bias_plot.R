 simulation_df_1 <- do_simulation(N_sim = 10, data_overlap_param = 0.5)
 simulation_df_2 <- do_simulation(N_sim = 10, data_overlap_param = 0.75)

 stats_df_1 <- analyse_simulations(simulation_df_1)$shared_results
 stats_df_2 <- analyse_simulations(simulation_df_2)$shared_results

 datasets <- list(stats_df_1, stats_df_2)
 facet_names = c("Overlap 0.5", "Overlap 0.75")

test_that("facet_bias_plot returns ggplot object", {
 plot <- facet_bias_plot(datasets = datasets,
                 facet_names = facet_names)

 expect_s3_class(plot, "ggplot")
})

test_that("function throws error when datasets and facet_names have different lengths", {
  expect_error(
    facet_bias_plot(
      datasets = datasets,
      facet_names = c("Group1")
    ),
    "The number of datasets must equal the number of facet names"
  )
})

test_that("facet_bias_plot visual snapshot", {
  skip_if_not_installed("vdiffr")

  plot <- facet_bias_plot(
    datasets = datasets,
    facet_names = c("Facet 1", "Facet 2"), 
    max_val = 2,
    title = "My Plot"
  )

  vdiffr::expect_doppelganger("facet_bias_plot_custom", plot)
})

