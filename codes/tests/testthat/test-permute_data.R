library(testthat)
library(data.table)

# Test case for permute_data function
test_that("permute_data works correctly for different types", {
  set.seed(123)

  # Create a simple dataset
  data <- data.frame(
    Y = c(1, 2, 3, 4, 5),
    A = factor(c(1, 1, 2, 2, 1)),
    B = factor(c(1, 2, 1, 2, 1))
  )
  data <- as.data.table(data)  # Convert to data.table

  # Test: Permuting factor A
  permuted_data_A <- permute_data(data, "main_A")

  # Check if factor A is shuffled and factor B and Y remain the same
  expect_true(any(permuted_data_A$A != data$A))  # A should be permuted (check for at least one difference)
  expect_equal(permuted_data_A$B, data$B)  # B should remain the same
  expect_equal(permuted_data_A$Y, data$Y)  # Y should remain the same

  # Test: Permuting factor B
  permuted_data_B <- permute_data(data, "main_B")

  # Check if factor B is shuffled and factor A and Y remain the same
  expect_equal(permuted_data_B$A, data$A)  # A should remain the same
  expect_true(any(permuted_data_B$B != data$B))  # B should be permuted (check for at least one difference)
  expect_equal(permuted_data_B$Y, data$Y)  # Y should remain the same

  # Test: Permuting the response variable Y
  permuted_data_Y <- permute_data(data, "interaction")

  # Check if Y is shuffled and factors A and B remain the same
  expect_equal(permuted_data_Y$A, data$A)  # A should remain the same
  expect_equal(permuted_data_Y$B, data$B)  # B should remain the same
  expect_true(any(permuted_data_Y$Y != data$Y))  # Y should be permuted (check for at least one difference)

  # Check that factors A and B have the correct levels after permutation
  expect_equal(levels(permuted_data_A$A), levels(data$A))
  expect_equal(levels(permuted_data_B$B), levels(data$B))

  # Ensure that Y remains a factor after the permutation for factor variables
  expect_equal(class(permuted_data_A$A), "factor")
  expect_equal(class(permuted_data_B$B), "factor")
})



