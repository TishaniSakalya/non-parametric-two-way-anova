library(testthat)

test_that("stratified_perm_test_sequential works correctly", {

  # Create a simple test data frame
  set.seed(123)
  n <- 20
  data <- data.frame(
    A = rep(c("A1", "A2"), each = n / 2),
    B = rep(c("B1", "B2"), times = n / 2),
    Y = rnorm(n)
  )

  # Run stratified_perm_test_sequential with a small number of permutations (B = 100)
  result <- stratified_perm_test_sequential(data, B = 100)

  # Check that the result is a list
  expect_type(result, "list")

  # Check for the expected elements in the result
  expect_true("eta_A" %in% names(result))
  expect_true("eta_B" %in% names(result))
  expect_true("eta_interaction" %in% names(result))
  expect_true("p_eta_A" %in% names(result))
  expect_true("p_eta_B" %in% names(result))
  expect_true("p_eta_interaction" %in% names(result))

  # Check that eta values are numeric (they can be 'double' or 'numeric')
  expect_type(result$eta_A, "double")
  expect_type(result$eta_B, "double")
  expect_type(result$eta_interaction, "double")

  # Check that p-values are between 0 and 1
  expect_true(result$p_eta_A >= 0 & result$p_eta_A <= 1)
  expect_true(result$p_eta_B >= 0 & result$p_eta_B <= 1)
  expect_true(result$p_eta_interaction >= 0 & result$p_eta_interaction <= 1)

  # Check that the function handles invalid B input (negative)
  expect_error(stratified_perm_test_sequential(data, B = -100), "B must be a positive integer")

  # Check that the function handles invalid B input (zero)
  expect_error(stratified_perm_test_sequential(data, B = 0), "B must be a positive integer")

  # Check that the function works with larger B (e.g., B = 1000)
  result_large_B <- stratified_perm_test_sequential(data, B = 1000)

  # Check the result is still correct with a larger number of permutations
  expect_type(result_large_B, "list")

  # Check that eta values are still numeric
  expect_type(result_large_B$eta_A, "double")
  expect_type(result_large_B$eta_B, "double")
  expect_type(result_large_B$eta_interaction, "double")

  # Check that p-values are between 0 and 1
  expect_true(result_large_B$p_eta_A >= 0 & result_large_B$p_eta_A <= 1)
  expect_true(result_large_B$p_eta_B >= 0 & result_large_B$p_eta_B <= 1)
  expect_true(result_large_B$p_eta_interaction >= 0 & result_large_B$p_eta_interaction <= 1)
})

