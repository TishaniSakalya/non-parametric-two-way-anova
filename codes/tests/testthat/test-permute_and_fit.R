library(testthat)

# Test case for permute_and_fit
test_that("permute_and_fit handles permutations correctly", {
  set.seed(123)

  # Generate a simple dataset
  data <- data.frame(
    Y = c(1, 2, 3, 4, 5),
    A = factor(c(1, 1, 2, 2, 1)),
    B = factor(c(1, 2, 1, 2, 1))
  )

  # Assuming you have a function to permute data
  permute_data <- function(data, type) {
    data$Y <- sample(data$Y)
    return(data)
  }

  # Run permute_and_fit with 10 permutations
  results <- permute_and_fit(data, type = "Y", B = 10)

  # Check that the result is a list with length equal to B
  expect_length(results, 10)

  # Check that each result in the list is a list with named elements
  expect_true(all(sapply(results, function(res) {
    is.list(res) && all(names(res) %in% c("eta_A", "eta_B", "eta_interaction"))
  })))

  # Check that the eta_A, eta_B, eta_interaction values are numeric
  expect_true(all(sapply(results, function(res) {
    is.numeric(res$eta_A) && is.numeric(res$eta_B) && is.numeric(res$eta_interaction)
  })))
})

