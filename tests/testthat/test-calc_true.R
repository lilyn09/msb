result <- calc_true()

test_that("calc_true returns correct result", {
  expect_true(is.numeric(result))
  expect_length(result, 1)
  expect_equal(result, 0.5)
})
