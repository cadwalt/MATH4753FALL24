# Test the "mu" component
test_that("myncurve returns correct mu", {
  result <- myncurve(mu = 0, sigma = 1, a = 2)
  expect_equal(result$mu, 0)  # Check if 'mu' is correct
})

# Test the "sigma" component
test_that("myncurve returns correct sigma", {
  result <- myncurve(mu = 0, sigma = 1, a = 2)
  expect_equal(result$sigma, 1)  # Check if 'sigma' is correct
})

# Test the "area" component
test_that("myncurve returns correct area", {
  result <- myncurve(mu = 0, sigma = 1, a = 2)
  expected_area <- pnorm(2, mean = 0, sd = 1)
  expect_equal(result$area, expected_area)  # Check if 'area' matches the expected value
})
