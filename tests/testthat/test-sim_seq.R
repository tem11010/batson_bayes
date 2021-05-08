load_path <- here::here("sim_seq.R")
source(load_path)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
