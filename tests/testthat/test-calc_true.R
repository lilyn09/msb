test_that("calc_true returns numeric value", {
  # Test basic functionality
  result <- calc_true(em_strength = 0.5, overlap_param = 0.5, shared_em = TRUE)
  
  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(is.finite(result))
})

test_that("calc_true handles different parameters", {
  # Test different parameter combinations
  result1 <- calc_true(em_strength = 0, overlap_param = 0.5, shared_em = TRUE)
  result2 <- calc_true(em_strength = 1, overlap_param = 0.5, shared_em = TRUE)
  result3 <- calc_true(em_strength = 0.5, overlap_param = 0, shared_em = TRUE)
  result4 <- calc_true(em_strength = 0.5, overlap_param = 1, shared_em = TRUE)
  result5 <- calc_true(em_strength = 0.5, overlap_param = 0.5, shared_em = FALSE)
  
  expect_type(result1, "double")
  expect_type(result2, "double")
  expect_type(result3, "double")
  expect_type(result4, "double")
  expect_type(result5, "double")
})

test_that("calc_true is deterministic", {
  # Same parameters should give same results
  result1 <- calc_true(em_strength = 0.3, overlap_param = 0.7, shared_em = TRUE)
  result2 <- calc_true(em_strength = 0.3, overlap_param = 0.7, shared_em = TRUE)
  
  expect_equal(result1, result2)
})

test_that("calc_true handles edge cases", {
  # Test extreme parameter values
  expect_no_error(calc_true(em_strength = 0, overlap_param = 0, shared_em = TRUE))
  expect_no_error(calc_true(em_strength = 0, overlap_param = 1, shared_em = TRUE))
  expect_no_error(calc_true(em_strength = 1, overlap_param = 0, shared_em = FALSE))
  expect_no_error(calc_true(em_strength = 1, overlap_param = 1, shared_em = FALSE))
})
