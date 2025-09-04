result <- calc_true(em_strength = 0.5, overlap_param = 0.5)

test_that("calc_true returns result in correct structure", {
  expect_true(is.numeric(result))
  expect_length(result, 1)
})



