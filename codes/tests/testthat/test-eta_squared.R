library(testthat)

test_that("eta_squared computes effect size correctly", {
  set.seed(123)
  # Generate a simple dataset
  data <- data.frame(
    Y = c(1, 2, 3, 4, 5),
    A = factor(c(1, 1, 2, 2, 1))
  )

  # Fit a linear model
  fit <- lm(Y ~ A, data = data)

  # Calculate eta-squared
  eta_sq <- eta_squared(fit)

  # Check that the result is a double (which is a valid numeric type)
  expect_type(eta_sq, "double")

  # Check that the eta-squared value is between 0 and 1
  expect_true(eta_sq >= 0 && eta_sq <= 1)

  # Check that the eta-squared value is correct
  ss_effect <- sum((fitted(fit) - mean(fit$model$Y))^2)
  ss_total <- sum((fit$model$Y - mean(fit$model$Y))^2)
  expected_eta_sq <- ss_effect / ss_total
  expect_equal(eta_sq, expected_eta_sq)
})
