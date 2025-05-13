library(testthat)

test_that("summarize_permutation_results works correctly", {

  # Create mock permutation result data
  set.seed(123)
  results <- list()
  for (i in 1:100) {
    results[[i]] <- list(
      eta_A = rnorm(1, mean = 0.5, sd = 0.1),
      eta_B = rnorm(1, mean = 0.3, sd = 0.1),
      eta_interaction = rnorm(1, mean = 0.2, sd = 0.05)
    )
  }

  # Run summarize_permutation_results
  summary_results <- summarize_permutation_results(results)

  # Check that the result is a list
  expect_type(summary_results, "list")

  # Check for the expected summary elements
  expect_true("summary_A" %in% names(summary_results))
  expect_true("summary_B" %in% names(summary_results))
  expect_true("summary_interaction" %in% names(summary_results))

  # Check that each summary contains four elements (mean, sd, ci_lower, ci_upper)
  expect_length(summary_results$summary_A, 4)
  expect_length(summary_results$summary_B, 4)
  expect_length(summary_results$summary_interaction, 4)

  # Check that the mean, sd, and CI bounds are numeric
  expect_type(summary_results$summary_A["mean"], "double")
  expect_type(summary_results$summary_A["sd"], "double")
  expect_type(summary_results$summary_A["ci_lower"], "double")  # Lower bound
  expect_type(summary_results$summary_A["ci_upper"], "double")  # Upper bound

  expect_type(summary_results$summary_B["mean"], "double")
  expect_type(summary_results$summary_B["sd"], "double")
  expect_type(summary_results$summary_B["ci_lower"], "double")  # Lower bound
  expect_type(summary_results$summary_B["ci_upper"], "double")  # Upper bound

  expect_type(summary_results$summary_interaction["mean"], "double")
  expect_type(summary_results$summary_interaction["sd"], "double")
  expect_type(summary_results$summary_interaction["ci_lower"], "double")  # Lower bound
  expect_type(summary_results$summary_interaction["ci_upper"], "double")  # Upper bound

  # Check that the 95% CI values are within the expected range
  expect_true(summary_results$summary_A["ci_lower"] <= summary_results$summary_A["mean"] && summary_results$summary_A["mean"] <= summary_results$summary_A["ci_upper"])
  expect_true(summary_results$summary_B["ci_lower"] <= summary_results$summary_B["mean"] && summary_results$summary_B["mean"] <= summary_results$summary_B["ci_upper"])
  expect_true(summary_results$summary_interaction["ci_lower"] <= summary_results$summary_interaction["mean"] && summary_results$summary_interaction["mean"] <= summary_results$summary_interaction["ci_upper"])

})
