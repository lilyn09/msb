 simulation_df_1 <- do_simulation(N_sim = 10, data_overlap_param = 0.5)
 simulation_df_2 <- do_simulation(N_sim = 10, data_overlap_param = 0.75)

 datasets <- list(simulation_df_1$results_df, simulation_df_2$results_df)
 facet_names = c("Overlap 0.5", "Overlap 0.75")

test_that("facet_coverage_plot returns ggplot object", {
 plot <- facet_coverage_plot(datasets = datasets,
                 facet_names = facet_names)

 expect_s3_class(plot, "ggplot")
})

test_that("function throws error when datasets and facet_names have different lengths", {
  expect_error(
    facet_coverage_plot(
      datasets = datasets,
      facet_names = c("Group1")
    ),
    "The number of datasets must equal the number of facet names"
  )
})

test_that("facet_coverage_plot visual snapshot", {
  skip_if_not_installed("vdiffr")

  plot <- facet_coverage_plot(
    datasets = datasets,
    facet_names = c("Facet 1", "Facet 2"), 
    title = "My Plot"
  )

  vdiffr::expect_doppelganger("facet_coverage_plot_custom", plot)
})
