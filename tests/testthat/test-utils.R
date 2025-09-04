test_that("calc_ci returns correct structure", {
  result <- calc_ci(3, 0.25, alpha = 0.05)

  expect_s3_class(result, "data.frame")
  
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 2)
  
  expect_true(all(c("ci_lower", "ci_upper") %in% names(result)))
  
  expect_true(is.numeric(result$ci_lower))
  expect_true(is.numeric(result$ci_upper))
})

test_that("calc_ci produces correct mathematical results", {
  x <- 2.5
  se <- 0.5
  alpha <- 0.05
  
  result <- calc_ci(x, se, alpha)
  
  z_value <- qnorm(1 - alpha / 2)  
  expected_lower <- x - z_value * se
  expected_upper <- x + z_value * se
  
  expect_equal(result$ci_lower, expected_lower, tolerance = 1e-12)
  expect_equal(result$ci_upper, expected_upper, tolerance = 1e-12)
})

test_that("calc_ci validates input lengths", {
  # Test mismatched vector lengths
  expect_error(
    calc_ci(c(1, 2), c(0.1, 0.2, 0.3)),
    "length\\(est\\) == length\\(se\\) is not TRUE"
  )
})

