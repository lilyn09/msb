plot <- facet_overlap_plot(mu_X1 = 0.5, sigma_X1 = 0.25, mu_X2 = 0.75, sigma_X2 = 0.4,
          N = 100, seed = 1234, x_axis_max = 4, y_axis_max = 4, x_axis_min = -1, y_axis_min = -2,
          plot_title = "test plot", overlap_params = c(1, 0.5))

test_that("facet_overlap_plot returns ggplot object", {
  expect_s3_class(plot, "ggplot")
})

test_that("facet_coverage_plot visual snapshot", {
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger("facet_overlap_plot", plot)
})
