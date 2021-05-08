test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that('no missing values', {
  expect_identical(a, na.omit(a))
})