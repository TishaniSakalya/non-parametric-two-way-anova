library(testthat)

# Create a sample dataset
data <- data.frame(
  Y = rnorm(100),
  A = factor(rep(1:2, 50)),
  B = factor(rep(1:2, each = 50))
)

# Number of bootstrap samples for testing
num_samples <- 100

# Test cases for bootstrap functions
test_that("Bootstrap functions return valid numeric results", {
  for (method in names(bootstrap_functions)) {
    results <- bootstrap_functions[[method]](data, num_samples)

    expect_type(results, "double")  # Should return a numeric vector
    expect_length(results, num_samples)  # Should have num_samples elements
    expect_false(any(is.na(results)))  # Should not contain NA values
  }
})

# Edge case: Check if functions handle empty data
test_that("Bootstrap functions handle empty datasets", {
  empty_data <- data.frame(Y = numeric(0), A = factor(), B = factor())
  for (method in names(bootstrap_functions)) {
    expect_error(bootstrap_functions[[method]](empty_data, num_samples))
  }
})

# Edge case: Check if functions handle non-numeric Y variable
test_that("Bootstrap functions handle non-numeric Y variable", {
  bad_data <- data
  bad_data$Y <- as.character(bad_data$Y)  # Convert Y to character
  for (method in names(bootstrap_functions)) {
    expect_error(bootstrap_functions[[method]](bad_data, num_samples))
  }
})
