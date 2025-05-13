library(testthat)

test_that("get_coef_name returns correct coefficient name", {

  set.seed(123)

  # Generate a simple dataset with explicit factor levels
  data <- data.frame(
    Y = c(1, 2, 3, 4, 5),
    A = factor(c(1, 1, 2, 2, 1), levels = c(1, 2)),  # Explicitly set factor levels
    B = factor(c(1, 2, 1, 2, 1), levels = c(1, 2))   # Explicitly set factor levels
  )

  # Fit a linear model
  fit <- lm(Y ~ A * B, data = data)

  # Test for the coefficient corresponding to 'A2' (since A1 is the reference level)
  coef_name_A <- get_coef_name(fit, "A2")
  expect_equal(coef_name_A, "A2")  # A2 will be the name of the coefficient for factor A level 2

  # Test for the coefficient corresponding to 'B2' (since B1 is the reference level)
  coef_name_B <- get_coef_name(fit, "B2")
  expect_equal(coef_name_B, "B2")  # B2 will be the name of the coefficient for factor B level 2

  # Test for a coefficient that doesn't exist
  expect_error(get_coef_name(fit, "NonexistentTerm"),
               "Coefficient NonexistentTerm not found in model")
})
