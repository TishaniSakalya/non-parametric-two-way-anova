library(testthat)

test_that("calculate_eta_squared works correctly", {
  set.seed(123)
  data <- data.frame(Y = c(1, 2, 3, 4, 5), A = factor(c(1, 1, 2, 2, 1)), B = factor(c(1, 2, 1, 2, 1)))

  fit <- lm(Y ~ A * B, data = data)
  eta_values <- calculate_eta_squared(fit, data)

  expect_type(eta_values, "list")
  expect_named(eta_values, c("eta_A", "eta_B", "eta_interaction"))
  expect_true(all(sapply(eta_values, is.numeric)))
  expect_true(all(sapply(eta_values, function(x) x >= 0 && x <= 1)))
})

test_that("calculate_eta_squared handles invalid input", {
  # Create a data frame with invalid data (non-numeric Y)
  data_invalid <- data.frame(Y = c("a", "b", "c", "d", "e"), A = factor(c(1, 1, 2, 2, 1)), B = factor(c(1, 2, 1, 2, 1)))

  expect_error(
    calculate_eta_squared(lm(Y ~ A * B, data_invalid), data_invalid),
    "Error: Y must be a numeric column in the dataset."
  )

  # Test for an empty dataset
  data_empty <- data.frame(Y = numeric(0), A = factor(), B = factor())

  # Check if the error is raised due to empty dataset
  expect_error(
    calculate_eta_squared(lm(Y ~ A * B, data_empty), data_empty),
    "Error: The dataset is empty."
  )
})



