context("testing SamplingDist")

test_that("str_length is number of characters", {
  ss <- SamplingDist(samples = 50)
  expect_equal(length(ss),50)
  })


