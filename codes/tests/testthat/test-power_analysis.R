library(testthat)
library(future)
library(future.apply)

test_that("power_analysis_permutation_future works correctly", {

  # Set up future backend for parallel processing (can be skipped in some cases)
  plan(multisession, workers = 2)

  # Reduce the number of simulations for quicker testing
  result <- power_analysis_permutation_future(
    n_per_group = 10,
    effect_size = 0.5,
    num_simulations = 10,  # Reduced number of simulations for quick testing
    alpha = 0.05,
    B = 100
  )

  # Check that the result is a list
  expect_is(result, "list")

  # Check for the expected elements in the result
  expect_true("estimated_power_A" %in% names(result))
  expect_true("estimated_power_B" %in% names(result))
  expect_true("estimated_power_interaction" %in% names(result))
  expect_true("num_simulations" %in% names(result))
  expect_true("alpha" %in% names(result))

  # Check that power estimates are between 0 and 1
  expect_true(result$estimated_power_A >= 0 & result$estimated_power_A <= 1)
  expect_true(result$estimated_power_B >= 0 & result$estimated_power_B <= 1)
  expect_true(result$estimated_power_interaction >= 0 & result$estimated_power_interaction <= 1)

  # Check for invalid B (negative)
  expect_error(power_analysis_permutation_future(
    n_per_group = 10,
    effect_size = 0.5,
    num_simulations = 10,  # Reduced to 10
    alpha = 0.05,
    B = -100
  ), "B must be a positive integer")

  # Check for invalid B (zero)
  expect_error(power_analysis_permutation_future(
    n_per_group = 10,
    effect_size = 0.5,
    num_simulations = 10,  # Reduced to 10
    alpha = 0.05,
    B = 0
  ), "B must be a positive integer")

  # Check for small sample size (n_per_group = 1)
  result_small_n <- power_analysis_permutation_future(
    n_per_group = 1,
    effect_size = 0.5,
    num_simulations = 10,  # Reduced to 10
    alpha = 0.05,
    B = 100
  )

  # Check that results are still returned for small sample size
  expect_true(result_small_n$estimated_power_A >= 0 & result_small_n$estimated_power_A <= 1)

  # Check for a very high number of simulations (to check for performance)
  result_large_simulations <- power_analysis_permutation_future(
    n_per_group = 10,
    effect_size = 0.5,
    num_simulations = 50,  # Reduced number of simulations for testing performance
    alpha = 0.05,
    B = 100
  )

  # Check the result is correct even for a large number of simulations
  expect_true(result_large_simulations$estimated_power_A >= 0 & result_large_simulations$estimated_power_A <= 1)

  # Reset the plan to avoid affecting other tests
  plan(sequential)
})
